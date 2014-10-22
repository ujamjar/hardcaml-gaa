# HardCaml + Guarded Atomic Actions

The aim is to play with the modular flow described in

http://ag-rs-www.informatik.uni-kl.de/publications/data/Rose05.pdf

This provides a compilation process for a hierarchical set of modules 
containing rules and methods and a kind of object oriented programming
model.

Chapter 3 describes FRL (flat rule language) and MRL (modular rule language).
A simplified compilation method of converting MRL to FRL is presented by
flattening.

Chapter 4 onwards describes a modular compilation scheme that avoids
flattening and allows modules to be compiled individually by means
of scheduling annotations.

Some of the modular compilation processes required are;

* when lifting - bubbles guards up to the top level
* implicit conditions 
* calculating intra method scheduling annotations (Figure 4-6)
* hierarchy flattening (ModMerge+Flatten Figure 3-4/5/6; MakeTree Figure 4-9)

Final conversion to rtl requires

* domain/range analysis (QoR)
* scheduling

