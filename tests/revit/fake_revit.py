"""A strict fake of the parts of the Revit API the Dynamo script touches.

Strict on purpose: documents, elements, parameters and connectors only expose
read-only members. Any attempt to touch a write API (Transaction,
TransactionManager, Parameter.Set, Document.Delete, ...) raises
WriteAttempt, so a test fails if the script ever tries to change the model.
"""
import sys
import types


class WriteAttempt(Exception):
    pass


class Strict(object):
    """Unknown attribute -> error naming it (catches API misuse)."""
    _allowed = ()

    def __getattr__(self, name):
        if name.startswith("Set") or name in ("Delete", "Regenerate", "Create"):
            raise WriteAttempt("script tried to call %s" % name)
        raise AttributeError("%s has no attribute %r"
                             % (type(self).__name__, name))


# ---- config: which Revit version we're pretending to be ---------------------
class Version(object):
    year = 2024


# ---- enums ------------------------------------------------------------------
class Enum(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name


def enum_class(name, members):
    return type(name, (object,), dict((m, Enum(name + "." + m)) for m in members))


BuiltInCategory = enum_class("BuiltInCategory",
                             ["OST_DuctTerminal", "OST_DuctCurves",
                              "OST_MechanicalEquipment", "OST_Walls"])
BuiltInParameter = enum_class("BuiltInParameter",
                              ["RBS_SYSTEM_CLASSIFICATION_PARAM",
                               "RBS_DUCT_FLOW_PARAM", "SYMBOL_NAME_PARAM"])
Domain = enum_class("Domain", ["DomainHvac", "DomainPiping",
                               "DomainElectrical", "DomainUndefined"])
DuctSystemType = enum_class("DuctSystemType",
                            ["SupplyAir", "ReturnAir", "ExhaustAir",
                             "OtherAir", "UndefinedSystemType", "Fitting"])

CAT_IDS = {BuiltInCategory.OST_DuctTerminal: -2008013,
           BuiltInCategory.OST_DuctCurves: -2008000,
           BuiltInCategory.OST_MechanicalEquipment: -2001140,
           BuiltInCategory.OST_Walls: -2000011}


# ---- ElementId: .IntegerValue <2024, both 2024-2025, .Value only 2026+ -----
class ElementId(Strict):
    def __init__(self, v):
        self._v = v

    @property
    def Value(self):
        if Version.year < 2024:
            raise AttributeError("ElementId has no attribute 'Value'")
        return self._v

    @property
    def IntegerValue(self):
        if Version.year >= 2026:
            raise AttributeError("ElementId has no attribute 'IntegerValue'")
        return self._v


class Category(Strict):
    def __init__(self, bic):
        self.Id = ElementId(CAT_IDS[bic])

    @staticmethod
    def GetCategory(doc, bic):
        return Category(bic)


class Parameter(Strict):
    def __init__(self, string=None, double=None, has_value=True):
        self._s, self._d, self.HasValue = string, double, has_value

    def AsString(self):
        return self._s

    def AsDouble(self):
        return self._d


class Connector(Strict):
    def __init__(self, domain, system_type=None, flow=0.0):
        self.Domain = domain
        self._st = system_type
        self._flow = flow

    @property
    def DuctSystemType(self):
        if self.Domain != Domain.DomainHvac:   # real API throws here
            raise Exception("InvalidOperationException: not a duct connector")
        return self._st

    @property
    def Flow(self):
        return self._flow


class ConnectorManager(Strict):
    def __init__(self, connectors):
        self.Connectors = list(connectors)


class MEPModel(Strict):
    def __init__(self, connectors):
        self.ConnectorManager = ConnectorManager(connectors)


class Element(Strict):
    def __init__(self, eid, bic=None, params=None, type_id=None):
        self.Id = ElementId(eid)
        self.Category = Category(bic) if bic is not None else None
        self._params = params or {}
        self._type_id = type_id

    def get_Parameter(self, bip):
        return self._params.get(bip)

    def GetTypeId(self):
        return ElementId(self._type_id if self._type_id is not None else -1)


class FamilyInstance(Element):
    def __init__(self, eid, bic, params=None, connectors=None, type_id=None):
        Element.__init__(self, eid, bic, params, type_id)
        self.MEPModel = MEPModel(connectors) if connectors is not None else None


class ElementType(Element):
    def __init__(self, eid, family, name):
        Element.__init__(self, eid, None,
                         {BuiltInParameter.SYMBOL_NAME_PARAM: Parameter(name)})
        self.FamilyName = family


class Group(Element):
    def __init__(self, eid, member_ids):
        Element.__init__(self, eid, None)
        self._m = member_ids

    def GetMemberIds(self):
        return [ElementId(i) for i in self._m]


class AssemblyInstance(Group):
    pass


class Units(Strict):
    pass


class SpecTypeId(object):
    AirFlow = Enum("SpecTypeId.AirFlow")


class UnitType(object):
    UT_HVAC_Airflow = Enum("UnitType.UT_HVAC_Airflow")


class UnitFormatUtils(object):
    calls = []

    @staticmethod
    def Format(units, spec, value, *flags):
        UnitFormatUtils.calls.append((spec, flags))
        if spec is SpecTypeId.AirFlow and len(flags) == 1:
            return "%.0f CFM" % (value * 60.0)       # project in CFM
        if spec is UnitType.UT_HVAC_Airflow and len(flags) == 2:
            return "%.0f CFM*" % (value * 60.0)
        raise TypeError("bad overload")


class Document(Strict):
    def __init__(self, elements):
        self._els = dict((e.Id._v, e) for e in elements)

    def GetElement(self, eid):
        return self._els.get(eid._v)

    def GetUnits(self):
        return Units()


class Selection(Strict):
    def __init__(self, ids):
        self._ids = ids

    def GetElementIds(self):
        return [ElementId(i) for i in self._ids]


class UIDocument(Strict):
    def __init__(self, doc, ids):
        self.Document = doc
        self.Selection = Selection(ids)


class TaskDialog(object):
    shown = []

    @staticmethod
    def Show(title, text):
        TaskDialog.shown.append((title, text))


class _DocMgr(object):
    Instance = None


class _UIApp(object):
    def __init__(self, uidoc):
        self.ActiveUIDocument = uidoc


def _forbidden(name):
    def boom(*a, **k):
        raise WriteAttempt("script used %s" % name)
    return boom


class _ForbiddenModule(types.ModuleType):
    """Module whose write-side classes explode when touched."""
    WRITE_NAMES = ("Transaction", "SubTransaction", "TransactionGroup",
                   "TransactionManager")

    def __getattr__(self, name):
        if name in self.WRITE_NAMES:
            raise WriteAttempt("script imported %s" % name)
        raise AttributeError(name)


def install(doc, selected_ids, year=2024):
    """Put fake clr/Autodesk/RevitServices modules into sys.modules."""
    Version.year = year
    TaskDialog.shown = []
    UnitFormatUtils.calls = []

    clr = types.ModuleType("clr")
    clr.AddReference = lambda name: None

    db = _ForbiddenModule("Autodesk.Revit.DB")
    for name in ("BuiltInCategory", "BuiltInParameter", "Category", "Domain",
                 "Group", "AssemblyInstance", "UnitFormatUtils", "ElementId"):
        setattr(db, name, globals()[name])
    if year >= 2021:
        db.SpecTypeId = SpecTypeId
    if year <= 2021:
        db.UnitType = UnitType

    mech = _ForbiddenModule("Autodesk.Revit.DB.Mechanical")
    mech.DuctSystemType = DuctSystemType
    ui = _ForbiddenModule("Autodesk.Revit.UI")
    ui.TaskDialog = TaskDialog

    rs = _ForbiddenModule("RevitServices")
    pers = _ForbiddenModule("RevitServices.Persistence")
    trans = _ForbiddenModule("RevitServices.Transactions")
    uidoc = UIDocument(doc, selected_ids)
    mgr = types.SimpleNamespace(CurrentDBDocument=doc,
                                CurrentUIApplication=_UIApp(uidoc))
    pers.DocumentManager = types.SimpleNamespace(Instance=mgr)

    autodesk = types.ModuleType("Autodesk")
    revit = types.ModuleType("Autodesk.Revit")
    autodesk.Revit = revit
    revit.DB = db
    revit.UI = ui
    db.Mechanical = mech
    rs.Persistence = pers
    rs.Transactions = trans
    sys.modules.update({
        "clr": clr, "Autodesk": autodesk, "Autodesk.Revit": revit,
        "Autodesk.Revit.DB": db, "Autodesk.Revit.DB.Mechanical": mech,
        "Autodesk.Revit.UI": ui, "RevitServices": rs,
        "RevitServices.Persistence": pers,
        "RevitServices.Transactions": trans})
