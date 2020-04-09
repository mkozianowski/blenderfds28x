# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

import bpy


# Shortcut

# TODO
# from . import bl_info
# module = bl_info['name']
# PKG = __package__
# mod = addon_utils.addons_fake_modules.get(PKG)
# mod.bl_info['show_expanded'] = True

# Supported file version

supported_file_version = 5, 0, 0

# Default SURFs

default_mas = {  # name: diffuse_color
    "Dummy Color1": ((1.0, 1.0, 1.0, 0.05),),  # white
    "Dummy Color2": ((1.0, 1.0, 0.0, 0.05),),  # yellow
    "Dummy Color3": ((1.0, 0.0, 1.0, 0.05),),  # purple
    "INERT": ((0.8, 0.8, 0.2, 1.0),),
    "HVAC": ((0.2, 0.2, 0.8, 0.5),),
    "MIRROR": ((1.0, 0.0, 1.0, 0.2),),
    "OPEN": ((0.2, 0.8, 0.8, 0.05),),
    "PERIODIC": ((1.0, 0.0, 1.0, 0.2),),
}

# Frequently used output QUANTITYs (FDS User's guide, table 16.3)


def get_quantity_items(qtype):
    items = []
    # Generated like this: (("[Heat] NET HEAT FLUX", "NET HEAT FLUX (kW/m²)", "Description...",) ...)
    for q in quantities:
        name, desc, units, allowed_qtype, subject = q
        if qtype in allowed_qtype:
            items.append((name, f"{subject} - {name} [{units}]", desc))
    items.sort(key=lambda k: k[1])
    return items


quantities = (  # name, description, units, qtype, subject
    ("ACTUATED SPRINKLERS", "Number of activated sprinklers", "", "D", "Det"),
    (
        "ADIABATIC SURFACE TEMPERATURE",
        "Adiabatic surface temperature (AST), a quantity that is representative of the heat flux to a solid surface",
        "°C",
        "B,D",
        "Wall",
    ),
    (
        "ASPIRATION",
        "Central detector of aspiration detection system",
        "%/m",
        "D",
        "Det",
    ),
    ("BACKGROUND PRESSURE", "Background pressure", "Pa", "D,I,P,S", "Pressure"),
    (
        "BACK WALL TEMPERATURE",
        "Temperature of the back of an EXPOSED surface.\nThe coordinates XYZ, and orientation IOR, refer to the front surface",
        "°C",
        "B,D",
        "Wall",
    ),
    ("BURNING RATE", "Mass loss rate of fuel", "kg/(m²·s)", "B,D", "Fire"),
    ("CHAMBER OBSCURATION", "Smoke detector chamber obscuration", "%/m", "D", "Det"),
    ("CONDUCTIVITY", "Thermal conductivity", "W/(m·K)", "D,I,P,S", "Gas"),
    (
        "CONVECTIVE HEAT FLUX",
        "Convective component of NET HEAT FLUX",
        "kW/m²",
        "B,D",
        "Heat",
    ),
    ("CPU TIME", "Elapsed CPU time since the start of the simulation", "s", "D", "Sim"),
    ("DENSITY", "Density", "kg/m³", "D,I,P,S", "Gas"),
    ("DEPOSITION VELOCITY", "Deposition velocity at the wall", "m/s", "B,D", "Wall"),
    ("DIVERGENCE", "Divergence", "1/s", "D,I,P,S", "Sim"),
    ("EXTINCTION COEFFICIENT", "", "1/m", "D,I,P,S", "Visibility"),
    (
        "FED",
        "The fractional effective dose index (FED), developed by Purser,\nis a commonly used measure of human incapacitation due to the combustion gases",
        "",
        "D",
        "Tenability",
    ),
    (
        "FIC",
        "The fractional irritant concentration (FIC), developed by Purser,\nrepresents the toxic effect which depends upon the immediate concentrations of irritants.",
        "",
        "D,S",
        "Tenability",
    ),
    (
        "GAUGE HEAT FLUX",
        "This quantity simulates a measurement made with a cold water heat flux gauge",
        "kW/m²",
        "B,D",
        "Det",
    ),
    (
        "HEAT FLOW",
        "Net flow of energy into or out of a planar surface",
        "kW",
        "D",
        "Heat",
    ),
    (
        "HEAT FLOW WALL",
        "Net flow of energy into or out of a solid boundary",
        "kW",
        "D",
        "Wall",
    ),
    (
        "NET HEAT FLUX",
        "Sum of the emitted and absorbed radiation at a solid surface",
        "kW/m²",
        "B,D",
        "Heat",
    ),
    ("HRR", "Heat release rate", "kW", "D", "Fire"),
    ("HRRPUA", "Heat release rate per unit area", "kW/m²", "D", "Fire"),
    ("HRRPUV", "Heat release rate per unit volume", "kW/m³", "D,I,P,S", "Fire"),
    ("INCIDENT HEAT FLUX", "Incident term of NET HEAT FLUX", "kW/m²", "B,D", "Heat"),
    (
        "INSIDE WALL TEMPERATURE",
        "Temperature inside a solid surface",
        "°C",
        "D",
        "Wall",
    ),
    ("INSIDE WALL DEPTH", "Depth inside a solid surface", "m", "D", "Wall"),
    (
        "ITERATION",
        "Number of time steps completed at the given time of the simulation",
        "",
        "D",
        "Sim",
    ),
    (
        "LAYER HEIGHT",
        "Layer height, location of the interface between the hot, smoke-laden upper layer and the cooler lower layer in a burning compartment",
        "m",
        "D",
        "Zones",
    ),
    (
        "LINK TEMPERATURE",
        "Defines a heat detector, which uses essentially the same activation algorithm as a sprinkler, without the water spray",
        "°C",
        "D",
        "Det",
    ),
    ("LOWER TEMPERATURE", "Lower layer temperature", "°C", "D", "Zones"),
    (
        "MASS FLOW",
        "Net flow of mass into or out of a planar surface",
        "kg/s",
        "D",
        "Gas",
    ),
    (
        "MASS FLOW WALL",
        "Net flow of mass into or out of a solid boundary",
        "kg/s",
        "D",
        "Wall",
    ),
    ("MASS FRACTION", "", "kg/kg", "D,I,P,S", "Gas"),
    ("MIXTURE FRACTION", "", "kg/kg", "D,I,P,S", "Gas"),
    ("NORMAL VELOCITY", "Wall normal velocity", "m/s", "D,B", "Wall"),
    ("OPTICAL DENSITY", "", "1/m", "D,I,P,S", "Visibility"),
    ("PATH OBSCURATION", "Beam detector path obscuration", "%", "D", "Det"),
    ("PRESSURE", "Perturbation pressure", "Pa", "D,I,P,S", "Pressure"),
    ("PRESSURE ZONE", "Pressure zone", "", "D,S", "Pressure"),
    (
        "RADIATIVE HEAT FLUX",
        "Radiative component of NET HEAT FLUX",
        "kW/m²",
        "B,D",
        "Heat",
    ),
    (
        "RADIATIVE HEAT FLUX GAS",
        "This records the radiative heat flux away from a solid surface",
        "kW/m²",
        "D",
        "Heat",
    ),
    (
        "RADIOMETER",
        "Similar to a GAUGE HEAT FLUX, this quantity measures only the radiative heat flux",
        "kW/m²",
        "B,D",
        "Det",
    ),
    ("RELATIVE HUMIDITY", "Relative humidity", "%", "D,I,P,S", "Gas"),
    ("SOLID CONDUCTIVITY", "Material component conductivity", "W/(m·K)", "D", "Wall"),
    ("SOLID DENSITY", "Material component density", "kg/m³", "D", "Wall"),
    (
        "SOLID SPECIFIC HEAT",
        "Material component specific heat",
        "kJ/(kg·K)",
        "D",
        "Wall",
    ),
    (
        "SPRINKLER LINK TEMPERATURE",
        "Compute the activation of the device using the standard RTI (Response Time Index) algorithm",
        "°C",
        "D",
        "Det",
    ),
    ("SURFACE DEPOSITION", "Surface deposition of SPEC_ID", "kg/m²", "B,D", "Wall"),
    ("TEMPERATURE", "", "°C", "D,I,P,S", "Gas"),
    (
        "THERMOCOUPLE",
        "Temperature of a modeled thermocouple.\nThe thermocouple temperature lags the true gas temperature by an amount determined mainly by its bead size.",
        "°C",
        "D",
        "Det",
    ),
    ("TIME", "Activation time", "s", "D", "Sim"),
    ("TIME STEP", "Duration of a simulation time step", "s", "D", "Sim"),
    ("U-VELOCITY", "Gas velocity component", "m/s", "D,I,P,S", "Gas"),
    ("V-VELOCITY", "Gas velocity component", "m/s", "D,I,P,S", "Gas"),
    ("W-VELOCITY", "Gas velocity component", "m/s", "D,I,P,S", "Gas"),
    ("UPPER TEMPERATURE", "Upper layer temperature", "°C", "D", "Zones"),
    ("VELOCITY", "Gas velocity", "m/s", "D,I,P,S", "Gas"),
    ("VISCOSITY", "Effective viscosity", "kg/(m·s)", "D,I,P,S", "Gas"),
    ("VISIBILITY", "Visibility through smoke", "m", "D,I,P,S", "Visibility"),
    (
        "VOLUME FLOW",
        "Net flow of volume into or out of a planar surface",
        "m³/s",
        "D",
        "Gas",
    ),
    (
        "VOLUME FLOW WALL",
        "Net flow of volume into or out of a solid boundary",
        "m³/s",
        "D",
        "Wall",
    ),
    ("VOLUME FRACTION", "", "mol/mol", "D,I,P,S", "Gas"),
    (
        "WALL CLOCK TIME",
        "Elapsed wall clock time since the start of the simulation",
        "s",
        "D",
        "Sim",
    ),
    (
        "WALL CLOCK TIME ITERATIONS",
        "Elapsed wall clock time since the start of the time stepping loop",
        "s",
        "D",
        "Sim",
    ),
    ("WALL TEMPERATURE", "Surface temperature", "°C", "B,D", "Wall"),
)
