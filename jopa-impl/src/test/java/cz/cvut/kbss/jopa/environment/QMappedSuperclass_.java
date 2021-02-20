package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.StaticMetamodel;

import java.net.URI;

@StaticMetamodel(QMappedSuperclass.class)
public class QMappedSuperclass_ {

    public static volatile Identifier<QMappedSuperclass, URI> uri;

    public static volatile SingularAttribute<QMappedSuperclass, String> label;

    public static volatile SingularAttribute<QMappedSuperclass, String> parentString;

    public static volatile SingularAttribute<QMappedSuperclass, OWLClassA> owlClassA;
}
