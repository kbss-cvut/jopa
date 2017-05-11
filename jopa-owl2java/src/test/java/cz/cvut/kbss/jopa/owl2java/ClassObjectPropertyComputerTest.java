/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.IntegrityConstraint;
import cz.cvut.kbss.jopa.ic.impl.IntegrityConstraintFactoryImpl;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

public class ClassObjectPropertyComputerTest {

    OWLOntology o;
    OWLDataFactory f;
    OWLClass cls1;
    OWLObjectProperty op;
    OWLClass cls2;

    IntegrityConstraintSet ics;

    @Before
    public void init() throws Exception {
        o = OWLManager.createOWLOntologyManager()
            .createOntology(IRI.create("http://examp.le/"));
        f = OWLManager.getOWLDataFactory();
        cls1 = f.getOWLClass(IRI.create("http://examp.le/c1"));
        op = f.getOWLObjectProperty(IRI.create("http://examp.le/p"));
        cls2 = f.getOWLClass(IRI.create("http://examp.le/c2"));

        ics = new IntegrityConstraintSet();
    }

    private void singleMinObjectParticipationConstraint(final OWLClass cls2, int card) throws Exception {
        final IntegrityConstraint ic1 = new IntegrityConstraintFactoryImpl()
            .MinObjectParticipationConstraint(cls1, op, cls2, card);
        ics.addIntegrityConstraint(ic1);

        final ClassObjectPropertyComputer c = new ClassObjectPropertyComputer(cls1, op, ics, o);

        Assert.assertEquals(1, c.getParticipationConstraints().size());
        Assert.assertEquals(Card.MULTIPLE, c.getCard());
        Assert.assertEquals(f.getOWLThing(), c.getFiller());
    }

    @Test
    public void testSingleMinConstraintWithCardZero() throws Exception {
        singleMinObjectParticipationConstraint(cls2, 0);
    }

    @Test
    public void testSingleMinConstraintWithCardOne() throws Exception {
        singleMinObjectParticipationConstraint(cls2, 1);
    }

    @Test
    public void testSingleMinConstraintWithCardTwo() throws Exception {
        singleMinObjectParticipationConstraint(cls2, 2);
    }
}