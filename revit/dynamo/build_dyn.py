"""Rebuild 'Supply Flow Total.dyn' from supply_flow_total.py.

Run after editing the script:  python3 revit/dynamo/build_dyn.py
"""
import json
import os
import uuid

HERE = os.path.dirname(os.path.abspath(__file__))
NS = uuid.UUID("6f1d2c3a-5b4e-4d7f-9a8b-1c2d3e4f5a6b")   # stable ids


def gid(name):
    return uuid.uuid5(NS, name).hex


def port(name, desc, key):
    return {"Id": gid(key), "Name": name, "Description": desc,
            "UsingDefaultValue": False, "Level": 2, "UseLevels": False,
            "KeepListStructure": False}


def build():
    with open(os.path.join(HERE, "supply_flow_total.py")) as f:
        code = f.read().replace("\r\n", "\n")

    toggle, python, watch = gid("toggle"), gid("python"), gid("watch")
    nodes = [
        {"ConcreteType": "CoreNodeModels.Input.BoolSelector, CoreNodeModels",
         "NodeType": "BooleanInputNode", "InputValue": True, "Id": toggle,
         "Inputs": [],
         "Outputs": [port("", "Boolean", "toggle.out")],
         "Replication": "Disabled",
         "Description": "Selection between a true and false."},
        {"ConcreteType": "PythonNodeModels.PythonNode, PythonNodeModels",
         "NodeType": "PythonScriptNode", "Code": code, "Engine": "CPython3",
         "VariableInputPorts": True, "Id": python,
         "Inputs": [port("IN[0]", "Input #0", "python.in0")],
         "Outputs": [port("OUT", "Result of the python script", "python.out")],
         "Replication": "Disabled",
         "Description": "Runs an embedded Python script."},
        {"ConcreteType": "CoreNodeModels.Watch, CoreNodeModels",
         "NodeType": "ExtensionNode", "Id": watch,
         "Inputs": [port("", "Node to show output from", "watch.in")],
         "Outputs": [port("", "Node output", "watch.out")],
         "Replication": "Disabled",
         "Description": "Visualize the node's output"},
    ]
    connectors = [
        {"Start": gid("toggle.out"), "End": gid("python.in0"),
         "Id": gid("c1"), "IsHidden": "False"},
        {"Start": gid("python.out"), "End": gid("watch.in"),
         "Id": gid("c2"), "IsHidden": "False"},
    ]

    def view(nid, name, x, y):
        return {"ShowGeometry": True, "Name": name, "Id": nid,
                "IsSetAsInput": False, "IsSetAsOutput": False,
                "Excluded": False, "X": x, "Y": y}

    graph = {
        "Uuid": str(uuid.uuid5(NS, "graph")),
        "IsCustomNode": False,
        "Description": "Totals the airflow of the supply air terminals "
                       "selected in Revit. Read-only: never changes the model.",
        "Name": "Supply Flow Total",
        "ElementResolver": {"ResolutionMap": {}},
        "Inputs": [], "Outputs": [],
        "Nodes": nodes, "Connectors": connectors,
        "Dependencies": [], "NodeLibraryDependencies": [], "Bindings": [],
        "View": {
            "Dynamo": {"ScaleFactor": 1.0, "HasRunWithoutCrash": True,
                       "IsVisibleInDynamoLibrary": True,
                       "Version": "2.10.1.3976", "RunType": "Manual",
                       "RunPeriod": "1000"},
            "Camera": {"Name": "Background Preview",
                       "EyeX": -17.0, "EyeY": 24.0, "EyeZ": 50.0,
                       "LookX": 12.0, "LookY": -13.0, "LookZ": -58.0,
                       "UpX": 0.0, "UpY": 1.0, "UpZ": 0.0},
            "NodeViews": [
                view(toggle, "Refresh (flip to re-run)", 0.0, 0.0),
                view(python, "Supply Flow Total", 250.0, 0.0),
                view(watch, "Result", 500.0, 0.0)],
            "Annotations": [], "X": 0.0, "Y": 0.0, "Zoom": 1.0},
    }
    out = os.path.join(HERE, "Supply Flow Total.dyn")
    with open(out, "w", newline="\r\n") as f:
        json.dump(graph, f, indent=2)
    return out


if __name__ == "__main__":
    print("wrote", build())
