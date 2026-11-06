# -*- coding: utf-8 -*-
"""Total the airflow of the selected supply air terminals.

Select air terminals (diffusers, grilles, registers) in any view, then click
the button. Only terminals whose system classification is Supply Air are
counted; return/exhaust terminals and anything that isn't an air terminal are
skipped and reported. If nothing is selected you're asked to pick terminals
(Finish on the options bar, or Esc to cancel).

Flow is read from the terminal's Flow parameter, falling back to the flow on
its duct connector. The total is shown in the project's airflow units, plus
CFM and L/s.
"""
from pyrevit import revit, DB, forms, script

doc = revit.doc

CFS_TO_CFM = 60.0          # Revit stores airflow internally in ft3/s
CFS_TO_LPS = 28.316846592


def is_air_terminal(el):
    cat = el.Category
    return cat is not None and \
        cat.Id.IntegerValue == int(DB.BuiltInCategory.OST_DuctTerminal)


def duct_connectors(el):
    mep = getattr(el, "MEPModel", None)
    mgr = getattr(mep, "ConnectorManager", None) if mep else None
    if mgr is None:
        return []
    out = []
    for c in mgr.Connectors:
        try:
            if c.Domain == DB.Domain.DomainHvac:
                out.append(c)
        except Exception:
            pass
    return out


def is_supply(el):
    """Supply Air terminal? Checks the duct connector, then the parameter."""
    for c in duct_connectors(el):
        try:
            if c.DuctSystemType == DB.Mechanical.DuctSystemType.SupplyAir:
                return True
            if c.DuctSystemType in (DB.Mechanical.DuctSystemType.ReturnAir,
                                    DB.Mechanical.DuctSystemType.ExhaustAir):
                return False
        except Exception:
            pass
    p = el.get_Parameter(DB.BuiltInParameter.RBS_SYSTEM_CLASSIFICATION_PARAM)
    text = (p.AsString() or "") if p else ""
    return "supply" in text.lower()


def flow_cfs(el):
    """Airflow in Revit internal units (ft3/s)."""
    p = el.get_Parameter(DB.BuiltInParameter.RBS_DUCT_FLOW_PARAM)
    if p is not None and p.HasValue:
        return p.AsDouble()
    total = 0.0
    for c in duct_connectors(el):
        try:
            total += c.Flow
        except Exception:
            pass
    return total


def format_flow(cfs):
    """Format in the project's airflow units (works Revit 2019 - current)."""
    units = doc.GetUnits()
    try:                                   # Revit 2021+
        return DB.UnitFormatUtils.Format(units, DB.SpecTypeId.AirFlow, cfs, False)
    except Exception:
        pass
    try:                                   # Revit 2020 and earlier
        return DB.UnitFormatUtils.Format(units, DB.UnitType.UT_HVAC_Airflow,
                                         cfs, False, False)
    except Exception:
        return "{:,.0f} CFM".format(cfs * CFS_TO_CFM)


def type_name(el):
    t = doc.GetElement(el.GetTypeId())
    if t is None:
        return "(no type)"
    fam = getattr(t, "FamilyName", "") or ""
    p = t.get_Parameter(DB.BuiltInParameter.SYMBOL_NAME_PARAM)
    name = (p.AsString() if p else None) or "(unnamed type)"
    return "{} : {}".format(fam, name) if fam else name


# --- get the elements -------------------------------------------------------
elements = list(revit.get_selection().elements)
if not elements:
    try:
        elements = revit.pick_elements_by_category(
            DB.BuiltInCategory.OST_DuctTerminal,
            message="Pick supply air terminals, then click Finish") or []
    except Exception:
        elements = []
    if not elements:
        forms.alert("Select some supply air terminals first.", exitscript=True)

# --- add them up ------------------------------------------------------------
total = 0.0
count = 0
not_supply = 0
not_terminal = 0
by_type = {}          # type name -> [count, flow]

for el in elements:
    if not is_air_terminal(el):
        not_terminal += 1
        continue
    if not is_supply(el):
        not_supply += 1
        continue
    q = flow_cfs(el)
    total += q
    count += 1
    row = by_type.setdefault(type_name(el), [0, 0.0])
    row[0] += 1
    row[1] += q

if count == 0:
    forms.alert("No supply air terminals in the selection.\n\n"
                "Skipped: {} return/exhaust terminal(s), {} other element(s)."
                .format(not_supply, not_terminal), exitscript=True)

# --- report -----------------------------------------------------------------
lines = ["Supply air terminals: {}".format(count),
         "Total flow: {}".format(format_flow(total)),
         "            ({:,.0f} CFM  |  {:,.1f} L/s)".format(
             total * CFS_TO_CFM, total * CFS_TO_LPS)]

if by_type:
    lines.append("")
    lines.append("By type:")
    for name in sorted(by_type):
        n, q = by_type[name]
        lines.append("  {} x {}  =  {}".format(n, name, format_flow(q)))

skipped = []
if not_supply:
    skipped.append("{} return/exhaust terminal(s)".format(not_supply))
if not_terminal:
    skipped.append("{} non-terminal element(s)".format(not_terminal))
if skipped:
    lines.append("")
    lines.append("Skipped: " + ", ".join(skipped))

forms.alert("\n".join(lines), title="Supply Air Flow Total")
