package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;

public class ConcreteEntityType<X> extends IdentifiableEntityType<X>{

    public ConcreteEntityType(String name, Class<X> javaType, IRI iri) {
        super(name, javaType, iri);
    }

    @Override
    public boolean isAbstract() {
        return false;
    }
}
