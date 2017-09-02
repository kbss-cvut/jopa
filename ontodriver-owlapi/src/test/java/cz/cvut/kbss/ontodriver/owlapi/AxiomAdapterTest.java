/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import com.google.common.base.Optional;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class AxiomAdapterTest {

    private static final String LANG = "en";

    private static final URI INDIVIDUAL = Generator.generateUri();
    private static final URI PROPERTY = Generator.generateUri();

    private AxiomAdapter adapter;

    @Before
    public void setUp() {
        this.adapter = new AxiomAdapter(new OWLDataFactoryImpl(), LANG);
    }

    @Test
    public void toOwlDataPropertyAxiomUsesLanguageTagSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createDataPropertyAssertion(PROPERTY, "cs", false), new Value<>("cestina"));
        final OWLAxiom axiom = adapter.toOwlDataPropertyAssertionAxiom(ax);
        final OWLDataPropertyAssertionAxiom dpAxiom = (OWLDataPropertyAssertionAxiom) axiom;
        final OWLLiteral literal = dpAxiom.getObject();
        assertEquals(ax.getValue().getValue(), literal.getLiteral());
        assertEquals("cs", literal.getLang());
    }

    @Test
    public void toOwlDataPropertyAxiomUsesGloballyConfiguredLanguageWhenItIsNotSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createDataPropertyAssertion(PROPERTY, false), new Value<>("english"));
        final OWLAxiom axiom = adapter.toOwlDataPropertyAssertionAxiom(ax);
        final OWLDataPropertyAssertionAxiom dpAxiom = (OWLDataPropertyAssertionAxiom) axiom;
        final OWLLiteral literal = dpAxiom.getObject();
        assertEquals(ax.getValue().getValue(), literal.getLiteral());
        assertEquals(LANG, literal.getLang());
    }

    @Test
    public void toOwlAnnotationPropertyAxiomUsesLanguageTagSpecifiedInAssertion() {
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
    public void toOwlAnnotationPropertyAxiomUsesGloballyConfiguredLanguageWhenItIsNotSpecifiedInAssertion() {
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(INDIVIDUAL),
                Assertion.createAnnotationPropertyAssertion(PROPERTY, false), new Value<>("english"));
        final OWLAxiom axiom = adapter.toOwlAnnotationPropertyAssertionAxiom(ax);
        final OWLAnnotationAssertionAxiom apAxiom = (OWLAnnotationAssertionAxiom) axiom;
        final Optional<OWLLiteral> literal = apAxiom.getValue().asLiteral();
        assertTrue(literal.isPresent());
        assertEquals(ax.getValue().getValue(), literal.get().getLiteral());
        assertEquals(LANG, literal.get().getLang());
    }
}