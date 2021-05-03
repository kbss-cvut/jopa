package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.StaticMetamodel;

import java.net.URI;

@StaticMetamodel(OWLClassC.class)
public class OWLClassC_ {

    public static volatile Identifier<OWLClassC, URI> uri;

    public static volatile ListAttribute<OWLClassC, OWLClassA> referencedList;

    public static volatile ListAttribute<OWLClassC, OWLClassA> simpleList;
}
