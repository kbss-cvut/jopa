package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.StaticMetamodel;

import java.net.URI;
import java.util.Map;

@StaticMetamodel(OWLClassB.class)
public class OWLClassB_ {

    public static volatile Identifier<OWLClassB, URI> uri;

    public static volatile SingularAttribute<OWLClassB, String> stringAttribute;

    public static volatile PropertiesSpecification<OWLClassB, Map, String, String> properties;
}
