# Hints for JOPA Usage
This page will list useful tips/hints that might help developers implement a JOPA-based solution.

## Making Inferred Attribute Writable 

### Problem
In JOPA, inferred attributes are not writable. However, sometimes one wants to both modify the property and fetch its values that are inferred.

An example would be the property skos:exactMatch. It might be both edited by an application (e.g. a SKOS editor) and inferred (e.g. because it is symmetric).

### Workaround
![image](https://user-images.githubusercontent.com/1140626/115024435-63310b00-9ec0-11eb-94cb-db4edda5b628.png)

1. Let's have a property `Q` . This property should be writable, e.g. `skos:exactMatch`.
2. Define a fresh property `P` in Your preferred namespace, e.g. `https://example.org/exactMatchInferred`. This one will be read-only.
3. Ensure `a P b` is inferred whenever `a Q b`. In case GraphDB is used as a JOPA backend storage, you can either add a statement `Q rdfs:subPropertyOf P` and benefit 
   from an existing RDFS inference rule set, or define your custom inference rule, as per https://graphdb.ontotext.com/documentation/standard/reasoning.html. 
4. Now, you can use the `exactMatchesInferred` JOPA field for reading the values (asserted and inferred) and `exactMatches` field for updating those which are not inferred.
