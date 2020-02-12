/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

public class AxiomSaverTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Mock
    private StorageConnector connectorMock;

    private AxiomSaver saver;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.saver = new AxiomSaver(connectorMock);
    }

    @Test
    public void saveAxiomsAddsClassAssertionToStorage() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion a = Assertion.createClassAssertion(false);
        final NamedResource type = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(a, new Value<>(type));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createResource(SUBJECT.toString()), arg.get(0).getSubject());
        assertEquals(ResourceFactory.createProperty(Vocabulary.RDF_TYPE), arg.get(0).getPredicate());
        assertEquals(ResourceFactory.createResource(type.getIdentifier().toString()), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsResourceStatementsForObjectPropertyAssertionAxioms() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        final NamedResource value = NamedResource.create(Generator.generateUri());
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createProperty(assertion.getIdentifier().toString()), arg.get(0).getPredicate());
        assertEquals(ResourceFactory.createResource(value.getIdentifier().toString()), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsLiteralStatementsForDatatypePropertyAssertionAxioms() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), "en", false);
        final Integer value = 117;
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createTypedLiteral(value), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsCreatesLanguageTaggedLiteralsForDatatypePropertyAssertionsWithLanguageTag() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final String lang = "en";
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), lang, false);
        final String value = "english text";
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createLangLiteral(value, lang), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsResourceStatementsForAnnotationPropertyAxiomsWithIriValue() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        final String value = Generator.generateUri().toString();
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createResource(value), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsLiteralStatementForAnnotationPropertyAxiomWithLiteralValue() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        final Integer value = 117;
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        assertEquals(ResourceFactory.createTypedLiteral(value), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsCorrectDateTimeLiteralForDateDataPropertyAxiomValue() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final Date value = new Date();
        descriptor.addAssertionValue(assertion, new Value<>(value));
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        final List<Statement> arg = captor.getValue();
        assertEquals(1, arg.size());
        final GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(value);
        assertEquals(ResourceFactory.createTypedLiteral(cal), arg.get(0).getObject());
    }

    @Test
    public void saveAxiomsAddsStatementsIntoCorrectContextWhenAssertionHasContext() {
        final URI context = Generator.generateUri();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final Integer value = 117;
        descriptor.addAssertionValue(assertion, new Value<>(value));
        descriptor.setAssertionContext(assertion, context);
        saver.saveAxioms(descriptor);
        verify(connectorMock).add(anyListOf(Statement.class), eq(context.toString()));
    }

    @Test
    public void saveAxiomsSkipsNullValues() {
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(SUBJECT);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertionValue(assertion, Value.nullValue());
        saver.saveAxioms(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        assertTrue(captor.getValue().isEmpty());
    }
}