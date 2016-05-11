# JOPA - Release Notes

## 0.7.7 - May 11, 2016
- Added the possibility to generate only vocabulary using OWL2Java, without the Java object model itself.

## 0.7.6 - April 15, 2016
- Added support for plain identifiers as values of Annotation and Object properties
    - E.g. when an entity has an object property pointing to another individual, but that individual is not mapped by any class in the object model, its URI can now be used as the property value

## 0.7.5 - March 31, 2016
- Use xsd:int for mapping Java Integer/int values to RDF. Most Java implementations (including JAXB) do it this way.

## 0.7.4 - March 21, 2016
- Implemented support for typed @Properties fields (i.e. @Properties field can now be a `Map<URI, Set<Object>>`, where values have corresponding types instead of plain strings)