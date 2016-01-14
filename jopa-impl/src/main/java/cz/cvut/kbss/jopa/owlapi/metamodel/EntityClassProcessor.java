package cz.cvut.kbss.jopa.owlapi.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.owlapi.EntityTypeImpl;

public class EntityClassProcessor {

    public <T> EntityTypeImpl<T> processEntityType(Class<T> cls) {
        final OWLClass c = cls.getAnnotation(OWLClass.class);

        if (c == null) {
            throw new MetamodelInitializationException("The class " + cls + " is not an OWLPersistence entity!");
        }

        checkForNoArgConstructor(cls);

        return new EntityTypeImpl<>(cls.getSimpleName(), cls, IRI.create(c.iri()));
    }

    private <T> void checkForNoArgConstructor(Class<T> cls) {
        try {
            cls.getDeclaredConstructor();
        } catch (NoSuchMethodException e) {
            throw new MetamodelInitializationException("Class " + cls + " is missing required no-arg constructor.");
        }
    }
}
