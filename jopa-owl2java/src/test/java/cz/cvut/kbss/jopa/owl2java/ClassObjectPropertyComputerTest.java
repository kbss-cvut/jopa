/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.IntegrityConstraint;
import cz.cvut.kbss.jopa.ic.api.IntegrityConstraintFactory;
import cz.cvut.kbss.jopa.ic.impl.IntegrityConstraintFactoryImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ClassObjectPropertyComputerTest {

    private OWLOntology o;
    private OWLDataFactory f;
    private OWLClass cls1;
    private OWLObjectProperty op;
    private OWLClass cls2;

    private IntegrityConstraintSet ics;
    private IntegrityConstraintFactory icFactory;

    @BeforeEach
    public void setUp() throws Exception {
        this.o = OWLManager.createOWLOntologyManager().createOntology(IRI.create("http://examp.le/"));
        this.f = OWLManager.getOWLDataFactory();
        this.cls1 = f.getOWLClass(IRI.create("http://examp.le/c1"));
        this.op = f.getOWLObjectProperty(IRI.create("http://examp.le/p"));
        this.cls2 = f.getOWLClass(IRI.create("http://examp.le/c2"));

        this.ics = new IntegrityConstraintSet();
        this.icFactory = new IntegrityConstraintFactoryImpl();
    }

    private void singleMinObjectParticipationConstraint(final OWLClass cls2, int card) {
        final IntegrityConstraint ic1 = icFactory.MinObjectParticipationConstraint(cls1, op, cls2, card);
        ics.addIntegrityConstraint(ic1);

        final ClassObjectPropertyComputer c = new ClassObjectPropertyComputer(cls1, op, ics, o);

        assertEquals(1, c.getParticipationConstraints().size());
        assertEquals(Card.MULTIPLE, c.getCard());
        assertEquals(f.getOWLThing(), c.getFiller());
    }

    @Test
    public void testSingleMinConstraintWithCardZero() {
        singleMinObjectParticipationConstraint(cls2, 0);
    }

    @Test
    public void testSingleMinConstraintWithCardOne() {
        singleMinObjectParticipationConstraint(cls2, 1);
    }

    @Test
    public void testSingleMinConstraintWithCardTwo() {
        singleMinObjectParticipationConstraint(cls2, 2);
    }

    @Test
    public void noMaxCardinalityConstraintReturnsCardMultiple() {
        ics.addIntegrityConstraint(icFactory.ObjectPropertyRangeConstraint(cls1, op, cls2));
        ics.addIntegrityConstraint(icFactory.MinObjectParticipationConstraint(cls1, op, cls2, 0));
        final ClassObjectPropertyComputer c = new ClassObjectPropertyComputer(cls1, op, ics, o);

        assertEquals(1, c.getParticipationConstraints().size());
        assertEquals(Card.MULTIPLE, c.getCard());
        assertEquals(cls2, c.getFiller());
    }

    @Test
    public void noCardinalityConstraintAndRangeConstraintReturnsCardMultiple() {
        ics.addIntegrityConstraint(icFactory.ObjectPropertyRangeConstraint(cls1, op, cls2));
        final ClassObjectPropertyComputer c = new ClassObjectPropertyComputer(cls1, op, ics, o);

        assertTrue(c.getParticipationConstraints().isEmpty());
        assertEquals(Card.MULTIPLE, c.getCard());
        assertEquals(cls2, c.getFiller());
    }
}
