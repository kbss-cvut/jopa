package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;

public class AbstractEntityType<X> extends EntityTypeImpl<X> {
    public AbstractEntityType(String name, Class<X> javaType, IRI iri) {
        super(name, javaType, iri);
    }

    @Override
    public boolean isAbstract() {
        return true;
    }
}
