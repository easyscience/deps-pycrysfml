import pytest
import CFML_api.crysfml_api
import CFML_api.powder_mod as powder_mod
import json

def test_atomic_mass():
    atoms = ['Fe', 'He', 'Ne', 'W']
    expected = [(55.84700012207031,), (4.002600193023682,), (20.179000854492188,), (183.85000610351562,)]
    for atom, e_value in zip(atoms, expected):
        value = CFML_api.crysfml_api.get_atomic_mass(atom)
        assert value == e_value

def load_data(fname : str) -> dict:
    with open(fname) as f:
        data = json.load(f)
    return data

def compute_pattern(data: dict):
    return powder_mod.simulation(data)

def test_pattern():
    file = 'Testing/Powder/Test_2/SrTiO3.json'
    data = compute_pattern(load_data(file))
