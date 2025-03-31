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

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.*;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static cz.cvut.kbss.jopa.owl2java.TestUtils.generateIri;
import static org.junit.jupiter.api.Assertions.*;

public class ContextDefinitionTest {

    private final ContextDefinition ctx = new ContextDefinition();

    private final OWLDataFactory df = new OWLDataFactoryImpl();

    @Test
    public void addAxiomAddsAxiomToContext() {
        final OWLAxiom axiom = df.getOWLDeclarationAxiom(df.getBooleanOWLDatatype());
        ctx.addAxiom(axiom);
        assertTrue(ctx.axioms.contains(axiom));
    }

    @Test
    public void addAxiomAddsClassInSignature() {
        final OWLClass cls = df.getOWLClass(generateIri());
        final OWLAxiom axiom = df.getOWLClassAssertionAxiom(cls, df.getOWLAnonymousIndividual());
        ctx.addAxiom(axiom);
        assertTrue(ctx.classes.contains(cls));
    }

    @Test
    public void addAxiomSkipsSequenceVocabularyClass() {
        final OWLClass cls = df.getOWLClass(SequencesVocabulary.c_List);
        final OWLAxiom axiom = df.getOWLClassAssertionAxiom(cls, df.getOWLAnonymousIndividual());
        ctx.addAxiom(axiom);
        assertFalse(ctx.classes.contains(cls));
    }

    @Test
    public void addAxiomAddsObjectPropertyInSignature() {
        final OWLObjectProperty op = df.getOWLObjectProperty(generateIri());
        final OWLAxiom axiom = df.getOWLObjectPropertyAssertionAxiom(op, df.getOWLNamedIndividual(generateIri()),
                df.getOWLAnonymousIndividual());
        ctx.addAxiom(axiom);
        assertTrue(ctx.objectProperties.contains(op));
    }

    @Test
    public void addAxiomAddsDataPropertyInSignature() {
        final OWLDataProperty dp = df.getOWLDataProperty(generateIri());
        final OWLAxiom axiom = df
                .getOWLDataPropertyAssertionAxiom(dp, df.getOWLAnonymousIndividual(), df.getOWLLiteral(117));
        ctx.addAxiom(axiom);
        assertTrue(ctx.dataProperties.contains(dp));
    }

    @Test
    public void addAxiomAddsAnnotationPropertyInSignature() {
        final OWLAnnotationProperty ap = df.getOWLAnnotationProperty(generateIri());
        final OWLAxiom axiom =
                df.getOWLAnnotationAssertionAxiom(ap, df.getOWLAnonymousIndividual(), df.getOWLLiteral(117));
        ctx.addAxiom(axiom);
        assertTrue(ctx.annotationProperties.contains(ap));
    }

    @Test
    public void addAxiomAddsNamedIndividualsInSignature() {
        final OWLObjectProperty op = df.getOWLObjectProperty(generateIri());
        final OWLNamedIndividual indOne = df.getOWLNamedIndividual(generateIri());
        final OWLNamedIndividual indTwo = df.getOWLNamedIndividual(generateIri());
        final OWLAxiom axiom = df.getOWLObjectPropertyAssertionAxiom(op, indOne, indTwo);
        ctx.addAxiom(axiom);
        assertTrue(ctx.individuals.contains(indOne));
        assertTrue(ctx.individuals.contains(indTwo));
    }

    @Test
    public void addAxiomKeepsStableOrderingOfProperties() {
        for (int i = 0; i < 5; i++) {
            final List<OWLObjectProperty> properties = new ArrayList<>();
            for (int j = 0; j < 10; j++) {
                final OWLObjectProperty op = df.getOWLObjectProperty(generateIri());
                properties.add(op);
                final OWLAxiom axiom =
                        df.getOWLObjectPropertyAssertionAxiom(op, df.getOWLNamedIndividual(generateIri()),
                                df.getOWLAnonymousIndividual());
                ctx.addAxiom(axiom);
            }
            Collections.sort(properties);
            final List<OWLObjectProperty> result = new ArrayList<>(ctx.objectProperties);
            assertEquals(properties, result);
            ctx.objectProperties.clear();
        }
    }
}
