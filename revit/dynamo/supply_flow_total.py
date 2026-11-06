# Supply Flow Total - Dynamo Python Script node (Revit 2021 - 2026)
#
# Totals the airflow of the SUPPLY air terminals in the current Revit selection
# and shows the result in a popup (and on the node's OUT port).
#
# READ-ONLY: this script never starts a transaction and never sets a parameter,
# so it cannot change the model. Revit refuses any model change made outside a
# transaction.
#
# - Air terminals whose system classification is Supply Air are counted.
# - Return/exhaust terminals and anything that isn't an air terminal are
#   skipped (and counted in the report).
# - Model groups and assemblies in the selection are opened up, so terminals
#   inside them are counted.
# - Flow comes from the terminal's Flow parameter (falls back to its duct
#   connector's flow) and is shown in the project's airflow units plus CFM
#   and L/s.
#
# IN[0] is an optional refresh toggle. Dynamo skips nodes whose inputs didn't
# change, so flip the toggle (or use Dynamo Player) to re-run after you change
# the selection.
#
# Works on IronPython 2.7, CPython3 and PythonNet3 engines: no f-strings, and
# ElementId values are read with .Value (2024+) or .IntegerValue (older).

import clr
clr.AddReference("RevitAPI")
clr.AddReference("RevitAPIUI")
clr.AddReference("RevitServices")

from Autodesk.Revit.DB import (BuiltInCategory, BuiltInParameter, Category,
                               Domain, Group, AssemblyInstance, UnitFormatUtils)
from Autodesk.Revit.DB.Mechanical import DuctSystemType
from Autodesk.Revit.UI import TaskDialog
from RevitServices.Persistence import DocumentManager

CFS_TO_CFM = 60.0           # Revit stores airflow internally in ft3/s
CFS_TO_LPS = 28.316846592
TITLE = "Supply Air Flow Total"


def commas(x, decimals):
    """1234.5 -> '1,234.5' (no format-spec commas: not safe on IronPython)."""
    s = ("%." + str(decimals) + "f") % x
    sign = ""
    if s.startswith("-"):
        sign, s = "-", s[1:]
    whole, dot, frac = s.partition(".")
    groups = []
    while len(whole) > 3:
        groups.insert(0, whole[-3:])
        whole = whole[:-3]
    groups.insert(0, whole)
    return sign + ",".join(groups) + dot + frac


def id_value(eid):
    """Integer value of an ElementId on any Revit version."""
    try:
        return eid.Value            # Revit 2024+ (IntegerValue removed in 2026)
    except AttributeError:
        return eid.IntegerValue     # Revit 2023 and earlier


def duct_connectors(el):
    mep = getattr(el, "MEPModel", None)
    mgr = getattr(mep, "ConnectorManager", None) if mep is not None else None
    if mgr is None:
        return []
    found = []
    for c in mgr.Connectors:
        try:
            if c.Domain == Domain.DomainHvac:
                found.append(c)
        except Exception:
            pass
    return found


def is_supply(el):
    """True if the terminal is on Supply Air (connector first, then parameter)."""
    for c in duct_connectors(el):
        try:
            st = c.DuctSystemType
        except Exception:
            continue
        if st == DuctSystemType.SupplyAir:
            return True
        if st == DuctSystemType.ReturnAir or st == DuctSystemType.ExhaustAir:
            return False
    p = el.get_Parameter(BuiltInParameter.RBS_SYSTEM_CLASSIFICATION_PARAM)
    text = p.AsString() if p is not None else None
    return "supply" in (text or "").lower()


def flow_cfs(el):
    """Airflow in Revit internal units (ft3/s)."""
    p = el.get_Parameter(BuiltInParameter.RBS_DUCT_FLOW_PARAM)
    if p is not None and p.HasValue:
        return p.AsDouble()
    total = 0.0
    for c in duct_connectors(el):
        try:
            total += c.Flow
        except Exception:
            pass
    return total


def format_flow(doc, cfs):
    """Flow in the project's airflow units, on any Revit version."""
    try:                                            # Revit 2021+
        from Autodesk.Revit.DB import SpecTypeId
        return UnitFormatUtils.Format(doc.GetUnits(), SpecTypeId.AirFlow,
                                      cfs, False)
    except Exception:
        pass
    try:                                            # Revit 2020 and earlier
        from Autodesk.Revit.DB import UnitType
        return UnitFormatUtils.Format(doc.GetUnits(), UnitType.UT_HVAC_Airflow,
                                      cfs, False, False)
    except Exception:
        pass
    return commas(cfs * CFS_TO_CFM, 0) + " CFM"


def type_name(doc, el):
    t = doc.GetElement(el.GetTypeId())
    if t is None:
        return "(no type)"
    p = t.get_Parameter(BuiltInParameter.SYMBOL_NAME_PARAM)
    name = (p.AsString() if p is not None else None) or "(unnamed type)"
    fam = getattr(t, "FamilyName", None)
    if fam:
        return "{0} : {1}".format(fam, name)
    return name


def expand(doc, ids):
    """Elements for the ids, opening up groups/assemblies (no duplicates)."""
    out = []
    seen = set()
    stack = list(ids)
    while stack:
        eid = stack.pop()
        key = id_value(eid)
        if key in seen:
            continue
        seen.add(key)
        el = doc.GetElement(eid)
        if el is None:
            continue
        if isinstance(el, Group) or isinstance(el, AssemblyInstance):
            stack.extend(list(el.GetMemberIds()))
            continue
        out.append(el)
    return out


def supply_flow_report(doc, selected_ids):
    """Returns (report text, total ft3/s, supply terminal count)."""
    if not selected_ids:
        return ("Nothing is selected.\n\nSelect supply air terminals in Revit, "
                "then run this again.", 0.0, 0)

    cat = Category.GetCategory(doc, BuiltInCategory.OST_DuctTerminal)
    terminal_cat = id_value(cat.Id) if cat is not None else None

    total = 0.0
    count = 0
    not_supply = 0
    not_terminal = 0
    by_type = {}            # type name -> [count, ft3/s]

    for el in expand(doc, selected_ids):
        c = el.Category
        if c is None or terminal_cat is None or id_value(c.Id) != terminal_cat:
            not_terminal += 1
            continue
        if not is_supply(el):
            not_supply += 1
            continue
        q = flow_cfs(el)
        total += q
        count += 1
        row = by_type.setdefault(type_name(doc, el), [0, 0.0])
        row[0] += 1
        row[1] += q

    skipped = []
    if not_supply:
        skipped.append("{0} return/exhaust terminal(s)".format(not_supply))
    if not_terminal:
        skipped.append("{0} non-terminal element(s)".format(not_terminal))

    if count == 0:
        lines = ["No supply air terminals in the selection."]
    else:
        lines = ["Supply air terminals: {0}".format(count),
                 "Total flow: {0}".format(format_flow(doc, total)),
                 "({0} CFM  |  {1} L/s)".format(
                     commas(total * CFS_TO_CFM, 0), commas(total * CFS_TO_LPS, 1)),
                 "",
                 "By type:"]
        for name in sorted(by_type):
            n, q = by_type[name]
            lines.append("  {0} x {1} = {2}".format(n, name,
                                                     format_flow(doc, q)))
    if skipped:
        lines.append("")
        lines.append("Skipped: " + ", ".join(skipped))
    return ("\n".join(lines), total, count)


# --- run ---------------------------------------------------------------------
_doc = DocumentManager.Instance.CurrentDBDocument
_uidoc = DocumentManager.Instance.CurrentUIApplication.ActiveUIDocument
_report, _total, _count = supply_flow_report(
    _doc, list(_uidoc.Selection.GetElementIds()))
TaskDialog.Show(TITLE, _report)
OUT = _report
