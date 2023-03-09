package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public abstract class MultipleInheritanceTestRunner extends BaseRunner {
    public MultipleInheritanceTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void entityCanBeFoundByBothParentTypes() {
        this.em = getEntityManager("entityCanBeFoundByBothParentTypes", false);

        URI id = URI.create("local");
        final OWLClassMultipleParents child = new OWLClassMultipleParents();
        child.setId(id);
        child.setStringAttribute("AttRVal");
        child.setPluralAnnotationProperty(Collections.singleton("seeet"));

        em.persist(child);
        em.clear();
        final OWLClassMultipleParents found = findRequired(OWLClassMultipleParents.class, id);
        em.clear();
        final OWLParentB parentBFound = findRequired(OWLParentB.class, id);
        em.clear();
        final OWLParentA parentAFound = findRequired(OWLParentA.class, id);

        assertEquals(child.getId(), found.getId());
        assertEquals(child.getStringAttribute(),parentBFound.getStringAttribute());
        assertEquals(child.getPluralAnnotationProperty(), parentAFound.getPluralAnnotationProperty());
    }

    @Test
    void annotatedMethodPassesDownAnnotationValues(){
        this.em = getEntityManager("annotatedMethodPassesDownAnnotationValues", false);
        URI id = URI.create("ID_VALUE");
        final OWLClassWithUnProperties subject = new OWLClassWithUnProperties();

        subject.setId(id);
        subject.setName("NAME_VALUE");

        em.persist(subject);
        em.clear();

        OWLClassWithUnProperties found =  em.find(OWLClassWithUnProperties.class,id);



        IRI namePropertyIRI=  em.getMetamodel().entity(OWLClassWithUnProperties.class).getDeclaredAttribute("name").getIRI();

        assertNotNull(found);
        assertEquals(subject.getName(),found.getName());
        assertEquals(subject.getId(),found.getId());
        assertEquals(Vocabulary.p_m_unannotated_name,namePropertyIRI.toString());


    }

}
