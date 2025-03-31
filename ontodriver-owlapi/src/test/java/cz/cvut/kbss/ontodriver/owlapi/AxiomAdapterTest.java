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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class AxiomAdapterTest {

    private static final URI INDIVIDUAL = Generator.generateUri();
    private static final URI PROPERTY = Generator.generateUri();

    private AxiomAdapter adapter;

    @BeforeEach
    void setUp() {
        this.adapter = new AxiomAdapter(new OWLDataFactoryImpl());
    }

    @Test
    void toOwlDataPropertyAxiomUsesLanguageTagSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createDataPropertyAssertion(PROPERTY, "cs", false), new Value<>("cestina"));
        final OWLAxiom axiom = adapter.toOwlDataPropertyAssertionAxiom(ax);
        final OWLDataPropertyAssertionAxiom dpAxiom = (OWLDataPropertyAssertionAxiom) axiom;
        final OWLLiteral literal = dpAxiom.getObject();
        assertEquals(ax.getValue().getValue(), literal.getLiteral());
        assertEquals("cs", literal.getLang());
    }

    @Test
    void toOwlDataPropertyAxiomCreatesSimpleLiteralWhenLanguageIsNotSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createDataPropertyAssertion(PROPERTY, false), new Value<>("english"));
        final OWLAxiom axiom = adapter.toOwlDataPropertyAssertionAxiom(ax);
        final OWLDataPropertyAssertionAxiom dpAxiom = (OWLDataPropertyAssertionAxiom) axiom;
        final OWLLiteral literal = dpAxiom.getObject();
        assertEquals(ax.getValue().getValue(), literal.getLiteral());
        assertFalse(literal.hasLang());
    }

    @Test
    void toOwlAnnotationPropertyAxiomUsesLanguageTagSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createAnnotationPropertyAssertion(PROPERTY, "cs", false), new Value<>("cestina"));
        final OWLAxiom axiom = adapter.toOwlAnnotationPropertyAssertionAxiom(ax);
        final OWLAnnotationAssertionAxiom apAxiom = (OWLAnnotationAssertionAxiom) axiom;
        final Optional<OWLLiteral> literal = apAxiom.getValue().asLiteral();
        assertTrue(literal.isPresent());
        assertEquals(ax.getValue().getValue(), literal.get().getLiteral());
        assertEquals("cs", literal.get().getLang());
    }

    @Test
    void toOwlAnnotationPropertyAxiomCreatesSimpleLiteralWhenLanguageIsNotSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createAnnotationPropertyAssertion(PROPERTY, false), new Value<>("english"));
        final OWLAxiom axiom = adapter.toOwlAnnotationPropertyAssertionAxiom(ax);
        final OWLAnnotationAssertionAxiom apAxiom = (OWLAnnotationAssertionAxiom) axiom;
        final Optional<OWLLiteral> literal = apAxiom.getValue().asLiteral();
        assertTrue(literal.isPresent());
        assertEquals(ax.getValue().getValue(), literal.get().getLiteral());
        assertFalse(literal.get().hasLang());
    }

    @Test
    void toOwlAnnotationPropertyAssertionAxiomCreatesSimpleLiteralForStringLiteralContainingColonAndAnnotationPropertyAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createAnnotationPropertyAssertion(PROPERTY, false), new Value<>("test:value"));
        final OWLAxiom axiom = adapter.toOwlAnnotationPropertyAssertionAxiom(ax);
        final OWLAnnotationAssertionAxiom apAxiom = (OWLAnnotationAssertionAxiom) axiom;
        final Optional<OWLLiteral> literal = apAxiom.getValue().asLiteral();
        assertTrue(literal.isPresent());
        assertEquals(ax.getValue().getValue(), literal.get().getLiteral());
        assertFalse(literal.get().hasLang());
    }
}
