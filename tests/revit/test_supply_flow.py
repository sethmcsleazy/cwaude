"""Tests for revit/dynamo/supply_flow_total.py against a strict fake Revit API.

Run:  python3 -m unittest discover -s tests/revit -v
"""
import ast
import os
import re
import sys
import time
import unittest

sys.path.insert(0, os.path.dirname(__file__))
import fake_revit as R  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
SCRIPT = os.path.join(ROOT, "revit", "dynamo", "supply_flow_total.py")
with open(SCRIPT) as f:
    CODE = f.read()

B = R.BuiltInParameter
HVAC = R.Domain.DomainHvac
ST = R.DuctSystemType
TYPES = [R.ElementType(900, "Titus TMS", "24x24 - 10in neck"),
         R.ElementType(901, "Titus 350RL", "24x12"),
         R.ElementType(902, "Linear Slot", "4ft 2-slot")]


def cfs(cfm):
    return cfm / 60.0


def terminal(eid, cfm, system="Supply Air", conn=ST.SupplyAir, type_id=900,
             flow_param=True, extra_connectors=()):
    params = {B.RBS_SYSTEM_CLASSIFICATION_PARAM: R.Parameter(system)}
    if flow_param:
        params[B.RBS_DUCT_FLOW_PARAM] = R.Parameter(double=cfs(cfm))
    connectors = None
    if conn is not None:
        connectors = [R.Connector(HVAC, conn, cfs(cfm))] + list(extra_connectors)
    return R.FamilyInstance(eid, R.BuiltInCategory.OST_DuctTerminal, params,
                            connectors, type_id)


def run(elements, selected, year=2024):
    doc = R.Document(list(elements) + TYPES)
    R.install(doc, selected, year)
    ns = {"IN": [True], "__name__": "__dynamo__"}
    exec(compile(CODE, SCRIPT, "exec"), ns)
    return ns


class Basics(unittest.TestCase):
    def test_three_supply_terminals(self):
        els = [terminal(1, 100), terminal(2, 150), terminal(3, 250, type_id=901)]
        ns = run(els, [1, 2, 3])
        self.assertEqual(ns["_count"], 3)
        self.assertAlmostEqual(ns["_total"] * 60, 500.0)
        self.assertEqual(len(R.TaskDialog.shown), 1)
        title, text = R.TaskDialog.shown[0]
        self.assertEqual(title, "Supply Air Flow Total")
        self.assertEqual(ns["OUT"], text)
        self.assertIn("Supply air terminals: 3", text)
        self.assertIn("Total flow: 500 CFM", text)
        self.assertIn("(500 CFM  |  236.0 L/s)", text)
        self.assertIn("2 x Titus TMS : 24x24 - 10in neck = 250 CFM", text)
        self.assertIn("1 x Titus 350RL : 24x12 = 250 CFM", text)
        self.assertNotIn("Skipped", text)

    def test_mixed_selection_only_counts_supply(self):
        els = [terminal(1, 400),
               terminal(2, 300, "Return Air", ST.ReturnAir),
               terminal(3, 200, "Exhaust Air", ST.ExhaustAir),
               R.FamilyInstance(4, R.BuiltInCategory.OST_DuctCurves),
               R.Element(5, R.BuiltInCategory.OST_Walls),
               R.FamilyInstance(6, R.BuiltInCategory.OST_MechanicalEquipment,
                                connectors=[R.Connector(HVAC, ST.SupplyAir, 9)])]
        ns = run(els, [1, 2, 3, 4, 5, 6])
        text = ns["OUT"]
        self.assertEqual(ns["_count"], 1)
        self.assertAlmostEqual(ns["_total"] * 60, 400.0)
        self.assertIn("Skipped: 2 return/exhaust terminal(s), "
                      "3 non-terminal element(s)", text)

    def test_nothing_selected(self):
        ns = run([terminal(1, 100)], [])
        self.assertEqual(ns["_count"], 0)
        self.assertIn("Nothing is selected", ns["OUT"])
        self.assertEqual(len(R.TaskDialog.shown), 1)

    def test_only_return_selected(self):
        ns = run([terminal(1, 100, "Return Air", ST.ReturnAir)], [1])
        self.assertIn("No supply air terminals in the selection.", ns["OUT"])
        self.assertIn("1 return/exhaust terminal(s)", ns["OUT"])

    def test_zero_flow_terminals_still_counted(self):
        ns = run([terminal(1, 0), terminal(2, 0)], [1, 2])
        self.assertEqual(ns["_count"], 2)
        self.assertIn("Total flow: 0 CFM", ns["OUT"])


class SupplyDetection(unittest.TestCase):
    def test_unconnected_terminal_uses_classification(self):
        t = terminal(1, 120, "Supply Air", ST.UndefinedSystemType)
        self.assertEqual(run([t], [1])["_count"], 1)

    def test_no_mep_model_uses_classification(self):
        t = terminal(1, 120, "Supply Air", conn=None)
        self.assertEqual(run([t], [1])["_count"], 1)

    def test_connector_beats_parameter(self):
        t = terminal(1, 120, "Supply Air", ST.ReturnAir)
        self.assertEqual(run([t], [1])["_count"], 0)

    def test_missing_classification_is_not_supply(self):
        t = terminal(1, 120, None, ST.UndefinedSystemType)
        ns = run([t], [1])
        self.assertEqual(ns["_count"], 0)

    def test_classification_case_insensitive(self):
        t = terminal(1, 120, "SUPPLY AIR", ST.OtherAir)
        self.assertEqual(run([t], [1])["_count"], 1)

    def test_electrical_connector_does_not_crash(self):
        # powered/fan terminal: has an electrical connector too; reading
        # DuctSystemType on it throws in the real API
        elec = R.Connector(R.Domain.DomainElectrical)
        t = terminal(1, 150, extra_connectors=[elec])
        ns = run([t], [1])
        self.assertEqual(ns["_count"], 1)
        self.assertAlmostEqual(ns["_total"] * 60, 150.0)


class FlowValue(unittest.TestCase):
    def test_falls_back_to_connector_flow(self):
        t = terminal(1, 175, flow_param=False)
        self.assertAlmostEqual(run([t], [1])["_total"] * 60, 175.0)

    def test_falls_back_when_param_has_no_value(self):
        t = terminal(1, 175)
        t._params[B.RBS_DUCT_FLOW_PARAM] = R.Parameter(double=None,
                                                        has_value=False)
        self.assertAlmostEqual(run([t], [1])["_total"] * 60, 175.0)

    def test_parameter_preferred_over_connector(self):
        t = terminal(1, 175)
        t.MEPModel.ConnectorManager.Connectors[0]._flow = cfs(999)
        self.assertAlmostEqual(run([t], [1])["_total"] * 60, 175.0)


class Selection(unittest.TestCase):
    def test_groups_and_assemblies_are_opened_without_double_counting(self):
        els = [terminal(1, 100), terminal(2, 200), terminal(3, 300),
               terminal(4, 50, "Return Air", ST.ReturnAir),
               terminal(5, 25),
               R.Group(10, [1, 2, 11]),
               R.Group(11, [3, 4, 1]),          # nested, repeats 1
               R.AssemblyInstance(12, [5])]
        ns = run(els, [10, 1, 12])            # 1 also selected directly
        self.assertEqual(ns["_count"], 4)
        self.assertAlmostEqual(ns["_total"] * 60, 625.0)
        self.assertIn("1 return/exhaust terminal(s)", ns["OUT"])

    def test_bad_ids_and_no_category_are_handled(self):
        els = [terminal(1, 100), R.Element(2, None)]
        ns = run(els, [1, 2, 777])            # 777 doesn't exist
        self.assertEqual(ns["_count"], 1)
        self.assertIn("1 non-terminal element(s)", ns["OUT"])

    def test_missing_type(self):
        ns = run([terminal(1, 100, type_id=12345)], [1])
        self.assertIn("1 x (no type) = 100 CFM", ns["OUT"])

    def test_large_selection(self):
        n = 20000
        els = [terminal(100000 + i, 125, type_id=900 + i % 3) for i in range(n)]
        t0 = time.time()
        ns = run(els, [100000 + i for i in range(n)])
        self.assertLess(time.time() - t0, 10)
        self.assertEqual(ns["_count"], n)
        self.assertAlmostEqual(ns["_total"] * 60, 125.0 * n, places=3)
        self.assertIn("(2,500,000 CFM", ns["OUT"])


class RevitVersions(unittest.TestCase):
    def check(self, year, expect_total_text):
        els = [terminal(1, 1000), terminal(2, 250, "Return Air", ST.ReturnAir),
               terminal(3, 300)]
        ns = run(els, [1, 2, 3], year=year)
        self.assertEqual(ns["_count"], 2, year)
        self.assertAlmostEqual(ns["_total"] * 60, 1300.0)
        self.assertIn(expect_total_text, ns["OUT"])

    def test_2020_old_units_api(self):
        self.check(2020, "Total flow: 1300 CFM*")

    def test_2021_both_apis(self):
        self.check(2021, "Total flow: 1300 CFM")

    def test_2022_2023(self):
        self.check(2022, "Total flow: 1300 CFM")
        self.check(2023, "Total flow: 1300 CFM")

    def test_2024_2025(self):
        self.check(2024, "Total flow: 1300 CFM")
        self.check(2025, "Total flow: 1300 CFM")

    def test_2026_no_integervalue(self):
        self.check(2026, "Total flow: 1300 CFM")

    def test_units_api_failure_falls_back(self):
        orig = R.UnitFormatUtils.Format
        try:
            R.UnitFormatUtils.Format = staticmethod(
                lambda *a: (_ for _ in ()).throw(Exception("boom")))
            self.check(2024, "Total flow: 1,300 CFM")
        finally:
            R.UnitFormatUtils.Format = orig


class ReadOnly(unittest.TestCase):
    """The script must never be able to change the model."""

    def test_no_write_apis_in_source(self):
        code = re.sub(r"#.*", "", CODE)       # ignore comments
        for word in ("Transaction", "TransactionManager", ".Set(", "Delete",
                     "Regenerate", "Create", "SetElementIds", "doc.Save",
                     "Close("):
            self.assertNotIn(word, code)

    def test_only_expected_imports(self):
        mods = set()
        for node in ast.walk(ast.parse(CODE)):
            if isinstance(node, ast.ImportFrom):
                mods.add(node.module)
            elif isinstance(node, ast.Import):
                mods.update(a.name for a in node.names)
        self.assertEqual(mods, {"clr", "Autodesk.Revit.DB",
                                "Autodesk.Revit.DB.Mechanical",
                                "Autodesk.Revit.UI",
                                "RevitServices.Persistence"})

    def test_selection_is_not_changed(self):
        # strict fakes raise WriteAttempt on any Set*/Delete/Transaction use;
        # a full run over every element kind must not trip them
        els = [terminal(1, 100), R.Group(10, [1]),
               R.Element(5, R.BuiltInCategory.OST_Walls)]
        run(els, [10, 5])


class IronPythonCompat(unittest.TestCase):
    """Dynamo on Revit 2021 and earlier runs IronPython 2.7."""

    def test_no_python3_only_syntax(self):
        tree = ast.parse(CODE)
        for node in ast.walk(tree):
            self.assertNotIsInstance(node, ast.JoinedStr, "f-string")
            self.assertNotIsInstance(node, ast.Nonlocal)
            self.assertNotIsInstance(node, (ast.AsyncFunctionDef, ast.Await))
            self.assertNotIsInstance(node, ast.NamedExpr, "walrus")
            if isinstance(node, ast.FunctionDef):
                self.assertFalse(node.args.kwonlyargs, node.name)
                self.assertIsNone(node.returns, node.name)
        self.assertNotIn("print(", CODE)
        self.assertNotRegex(CODE, r"\{[0-9]*:,")   # format-spec commas

    def test_commas(self):
        ns = run([], [])
        c = ns["commas"]
        self.assertEqual(c(0, 0), "0")
        self.assertEqual(c(999, 0), "999")
        self.assertEqual(c(1000, 0), "1,000")
        self.assertEqual(c(1234567.891, 1), "1,234,567.9")
        self.assertEqual(c(-1234.5, 1), "-1,234.5")
        self.assertEqual(c(999.96, 1), "1,000.0")


class DynamoGraph(unittest.TestCase):
    """The .dyn must embed exactly the tested script and be wired correctly."""

    def setUp(self):
        import json
        with open(os.path.join(ROOT, "revit", "dynamo",
                               "Supply Flow Total.dyn")) as f:
            self.g = json.load(f)

    def test_embeds_current_script(self):
        py = [n for n in self.g["Nodes"] if n["NodeType"] == "PythonScriptNode"]
        self.assertEqual(len(py), 1)
        self.assertEqual(py[0]["Code"], CODE.replace("\r\n", "\n"),
                         "run python3 revit/dynamo/build_dyn.py")

    def test_wiring(self):
        outs = {p["Id"] for n in self.g["Nodes"] for p in n["Outputs"]}
        ins = {p["Id"] for n in self.g["Nodes"] for p in n["Inputs"]}
        for c in self.g["Connectors"]:
            self.assertIn(c["Start"], outs)
            self.assertIn(c["End"], ins)
        # every input port is connected (Dynamo won't run a node otherwise)
        self.assertEqual(ins, {c["End"] for c in self.g["Connectors"]})
        ids = [n["Id"] for n in self.g["Nodes"]]
        self.assertEqual(sorted(ids),
                         sorted(v["Id"] for v in self.g["View"]["NodeViews"]))
        self.assertEqual(self.g["View"]["Dynamo"]["RunType"], "Manual")


if __name__ == "__main__":
    unittest.main()
