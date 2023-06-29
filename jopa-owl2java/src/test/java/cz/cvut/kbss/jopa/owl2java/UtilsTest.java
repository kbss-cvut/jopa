/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.exception.UnsupportedICException;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.vocab.OWLFacet;
import org.semanticweb.owlapi.vocab.XSDVocabulary;

import static org.junit.jupiter.api.Assertions.assertThrows;

public class UtilsTest {

    private final OWLDataFactory f = OWLManager.getOWLDataFactory();

    @Test
    public void testSucceedOnClass() {
        Utils.ensureClass(f.getOWLClass(IRI.create("http://examp.le/cls")));
    }

    @Test
    public void testFailOnClassExpression() {
        assertThrows(UnsupportedICException.class,
                () -> Utils.ensureClass(f.getOWLObjectComplementOf(f.getOWLClass(IRI.create("http://examp.le/cls")))));
    }

    @Test
    public void testSucceedOnObjectProperty() {
        Utils.ensureObjectProperty(f.getOWLObjectProperty(IRI.create("http://examp.le/prop")));
    }

    @Test
    public void testFailOnObjectPropertyExpression() {
        assertThrows(UnsupportedICException.class, () -> Utils.ensureObjectProperty(
                f.getOWLObjectInverseOf(f.getOWLObjectProperty(IRI.create("http://examp.le/prop")))));
    }

    @Test
    public void testSucceedOnDataProperty() {
        Utils.ensureDataProperty(f.getOWLDataProperty(IRI.create("http://examp.le/prop")));
    }

    @Test
    public void testSucceedOnDatatype() {
        Utils.ensureDatatype(f.getOWLDatatype(XSDVocabulary.STRING.getIRI()));
    }

    @Test
    public void testFailOnDataRange() {
        assertThrows(UnsupportedICException.class, () -> Utils.ensureDatatype(
                f.getOWLDatatypeRestriction(f.getIntegerOWLDatatype(),
                        f.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, 10))));
    }
}
