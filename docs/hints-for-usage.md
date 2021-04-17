# Hints for JOPA Usage
This page will list useful tips/hints that might help developers implement a JOPA-based solution.

## Making Inferred Attribute Writable 

### Problem
In JOPA, inferred attributes are not writable. However, sometimes one wants to both modify the property and fetch its values that are inferred.

An example would be the property `skos:exactMatch`. It might be both edited by an application (e.g. a SKOS editor) and inferred (e.g. because it is symmetric).

### Workaround
![image](https://user-images.githubusercontent.com/1140626/115122818-44af3a80-9fba-11eb-9d8e-13a351266825.png)

1. Let's have a property `Q`, e.g. `skos:exactMatch`.
2. Define a fresh property `P` in Your preferred namespace, e.g. `https://example.org/exactMatchAsserted`. This one will be writeable, leaving `Q` read-only.
3. Ensure `a Q b` is inferred whenever `a P b`. In case GraphDB is used as a JOPA backend storage, you can either add a statement `P rdfs:subPropertyOf Q` and benefit from an existing RDFS inference rule set, or define your custom inference rule, as per https://graphdb.ontotext.com/documentation/standard/reasoning.html. 
4. Now, you can use the `exactMatchesAsserted` JOPA field for updating the non-inferred values, and `exactMatches` field for reading all the values (asserted and inferred).
