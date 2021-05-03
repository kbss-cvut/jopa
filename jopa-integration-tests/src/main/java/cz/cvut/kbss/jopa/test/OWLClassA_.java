package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.StaticMetamodel;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;

import java.net.URI;

/**
 * Manually created static metamodel for testing purposes.
 */
@StaticMetamodel(OWLClassA.class)
public class OWLClassA_ {

    public static volatile Identifier<OWLClassA, URI> uri;

    public static volatile SingularAttribute<OWLClassA, String> stringAttribute;

    public static volatile TypesSpecification<OWLClassA, String> types;
}
