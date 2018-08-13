package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.IntegrityConstraintFactory;
import cz.cvut.kbss.jopa.ic.impl.IntegrityConstraintFactoryImpl;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ClassDataPropertyComputerTest {

    private OWLOntology ontology;
    private OWLDataFactory dataFactory;
    private OWLClass cls;
    private OWLDataProperty dp;

    private IntegrityConstraintSet ics;
    private IntegrityConstraintFactory icFactory;

    @Before
    public void setUp() throws Exception {
        this.ontology = OWLManager.createOWLOntologyManager().createOntology(IRI.create("http://examp.le/"));
        this.dataFactory = OWLManager.getOWLDataFactory();
        this.cls = dataFactory.getOWLClass(IRI.create("http://examp.le/c"));
        this.dp = dataFactory.getOWLDataProperty(IRI.create("http://examp.le/p"));

        this.ics = new IntegrityConstraintSet();
        this.icFactory = new IntegrityConstraintFactoryImpl();
    }

    @Test
    public void noMaxCardinalityConstraintReturnsCardMultiple() {
        ics.addIntegrityConstraint(icFactory.DataPropertyRangeConstraint(cls, dp, dataFactory.getIntegerOWLDatatype()));
        ics.addIntegrityConstraint(
                icFactory.MinDataParticipationConstraint(cls, dp, dataFactory.getIntegerOWLDatatype(), 0));
        final ClassDataPropertyComputer comp = new ClassDataPropertyComputer(cls, dp, ics, ontology);

        assertEquals(1, comp.getParticipationConstraints().size());
        assertEquals(Card.MULTIPLE, comp.getCard());
        assertEquals(dataFactory.getIntegerOWLDatatype(), comp.getFiller());
    }

    @Test
    public void noCardinalityConstraintAndRangeConstraintReturnsCardMultiple() {
        ics.addIntegrityConstraint(icFactory.DataPropertyRangeConstraint(cls, dp, dataFactory.getIntegerOWLDatatype()));
        final ClassDataPropertyComputer comp = new ClassDataPropertyComputer(cls, dp, ics, ontology);

        assertTrue(comp.getParticipationConstraints().isEmpty());
        assertEquals(Card.MULTIPLE, comp.getCard());
        assertEquals(dataFactory.getIntegerOWLDatatype(), comp.getFiller());
    }
}