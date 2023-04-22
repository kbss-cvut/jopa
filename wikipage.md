# JOPA and multiple inheritance 

Multiple inheritance is now supported in JOPA. 
Please see text below, how to use.


## Usage

As Java does not directly support multiple inheritance,
an entity class cannot extend multiple entity classes (or mapped superclasses) at once.

JOPA takes advantage of interfaces, as a class can implement multiple interfaces at once.
To support multiple inheritance, it is possible to declare interfaces as entity classes in JOPA with ``@OWLClass`` annotation, 
same as plain Java classes.
A child class can then implement multiple of these interface entity classes,
along with almost all features of single inheritance.

The use of interface entity classes has some caveats that come from Java interface constraints.

- It cannot be directly instantiated, as it is interface. 
- It cannot declare any non-static non-final properties. 
  Child classes cannot inherit any attributes from these entity classes .  

The second caveat is partly omitted by using property annotations on methods.

### Property annotations on methods

In order to reduce code duplication, and mistakes from duplication,
JOPA supports declaring property annotations (``OWLDataProperty,OWLObjectProperty and OWLAnnotationProperty``)
on getters and setters. 

This means that the property annotation along with IRI and other parameters can be declared
once in parent entity class. 
All non-abstract children entity classes then only need to implement the method 
and declare field without the need for any annotations, as the field will _inherit_ property annotation from the annotated method. 


The assignment of field to annotated method starts, when unannotated field is discovered during metamodel creation.
The hierarchy is searched bottom up, looking for annotated methods in parent entity classes.
If annotated method is found, the field name is extracted from the method name based on Java naming conventions.
If name matches name of field, the field is processed with property annotations from method.

## Examples

### Simple hierarchy
    
    @OWLClass(iri = Vocabulary.CLASS_BASE + "AParentI")
    interface AParentI {}
  
    @OWLClass(iri = Vocabulary.CLASS_BASE + "BParentI")
    interface BParentI {}
  
    @OWLClass(iri = Vocabulary.CLASS_BASE + "InterfaceChild")
    class InterfaceChild implements AParentI, BParentI {
        @Id
        private URI uri;
    }

In code example above, a simple hierarchy is defined, this hierarchy mirrors some arbitrary topology.
In a Java's single inheritance, a decision would have to be made, which class would be a parent,
and which would be omitted from hierarchy.
However, this would disconnect Java model from model in underlying storage, 
which is problematic.


This example shows power of polymorphic behaviour.
Storage can be queried using ``<T> T EntityManager.find(final Class<T> entityClass, final Object identifier)``
method. EntityClass argument is a Java class to look for and return.
With multiple inheritance, we can pass any of the three classes ``InterfaceChild,BParentI`` and ``AParentI``,
with valid identifier, and have returned instance of ``InterfaceChild``, as in example below. 
    
    final InterfaceChild child = new InterfaceChild();
    child.setId(id);
    
    em.persist(child);
    
    OWLChildClassA foundChild = em.find(InterfaceChild.class, id);
    
    AParentI parentBFound = em.find(AParentI.class, id);
    
    BParentI parentAFound = em.find(BParentI.class, id);


### Annotated methods

    @OWLClass(iri = Vocabulary.BarInterface)
    public interface BarInterface {
        @OWLDataProperty(iri = Vocabulary.foo)
        String getFoo();
    }

    @OWLClass(iri = Vocabulary.BarChild)
    public class BarChild implements BarInterface {
        protected String foo;

        @Override
        public String getFoo() {return foo;}
    }

This hierarchy contains field ``foo`` which _inherits_ property annotation from method ``getFoo()``.


#### Two parents with same properties 

    @OWLClass(iri = Vocabulary.ParentA)
    public interface ParentA {
        @OWLDataProperty(iri = Vocabulary.name)
        void setName(String name);
    }

    @OWLClass(iri = Vocabulary.ParentB)
    public interface ParentB {
        @OWLDataProperty(iri = Vocabulary.name)
        void setName(String name);
    }

    @OWLClass(iri = Vocabulary.BarChild)
    public class Child implements ParentA,ParentB {
        protected String name;

        @Override
        public void setName(String name){this.name = name;}
    }

A field can _inherit_ annotations from multiple methods, as long as the annotations are equal.
If annotations are not equal, an exception is thrown during metamodel creation.


### Other examples
For more examples, see integration tests in ``MultipleInheritanceTestRunner.java`` in ``/jopa-integration-tests/src/main/java/cz/cvut/kbss/jopa/test/runner`` or
[JOPA examples](https://github.com/kbss-cvut/jopa-examples) repository.






