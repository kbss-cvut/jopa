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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class SingularAnnotationPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private MetamodelMocks mocks;

    private Descriptor descriptor = new EntityDescriptor();

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final Configuration configuration = new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.LANG, "en"));
        when(mapperMock.getConfiguration()).thenReturn(configuration);
        this.mocks = new MetamodelMocks();
    }

    @Test
    public void addAxiomValueAddsStringAnnotationValue() throws Exception {
        final String str = "stringValue";
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(PK), annotationForN(), new Value<>(str));

        strategy.addValueFromAxiom(ax);
        final OWLClassN n = new OWLClassN();
        strategy.buildInstanceFieldValue(n);
        assertEquals(str, n.getAnnotationProperty());
    }

    private SingularAnnotationPropertyStrategy<OWLClassN> forN(SingularAttribute<OWLClassN, ?> att) {
        return new SingularAnnotationPropertyStrategy<>(
                mocks.forOwlClassN().entityType(), att, descriptor, mapperMock);
    }

    private Assertion annotationForN() {
        final URI uri = mocks.forOwlClassN().annotationAttribute().getIRI().toURI();
        return Assertion.createAnnotationPropertyAssertion(uri, false);
    }

    @Test
    public void addAxiomValueSkipsAxiomWhoseValueDoesNotMatchTargetFieldType() throws Exception {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final Axiom<Integer> ax = new AxiomImpl<>(NamedResource.create(PK), annotationForN(), new Value<>(117));

        strategy.addValueFromAxiom(ax);
        final OWLClassN n = new OWLClassN();
        strategy.buildInstanceFieldValue(n);
        assertNull(n.getAnnotationProperty());
    }

    @Test
    public void addAxiomValueAddsPlainIdentifierValueOfAnnotationProperty() throws Exception {
        final URI value = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationValue");
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(
                mocks.forOwlClassN().annotationUriAttribute());
        final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(PK), annotationWithUriForN(),
                new Value<>(NamedResource.create(value)));

        strategy.addValueFromAxiom(ax);
        final OWLClassN n = new OWLClassN();
        strategy.buildInstanceFieldValue(n);
        assertEquals(value, n.getAnnotationUri());
    }

    private Assertion annotationWithUriForN() {
        final URI uri = mocks.forOwlClassN().annotationUriAttribute().getIRI().toURI();
        return Assertion.createAnnotationPropertyAssertion(uri, false);
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void addAxiomValueThrowsIntegrityConstraintsViolationWhenAnotherValueIsAlreadySet() throws Exception {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final List<Axiom<String>> axioms = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            axioms.add(new AxiomImpl<>(NamedResource.create(PK), annotationForN(), new Value<>("String" + i)));
        }

        axioms.forEach(strategy::addValueFromAxiom);
    }

    @Test
    public void buildAxiomsExtractsStringValueOfAnnotationPropertyField() throws Exception {
        final String str = "stringValue";
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(PK.toString());
        n.setAnnotationProperty(str);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(n, builder);

        final Value<?> val = getValueForAssertion(builder, annotationForN());
        assertEquals(str, val.getValue());
    }

    private Value<?> getValueForAssertion(AxiomValueGatherer builder, Assertion assertion) throws Exception {
        final AxiomValueDescriptor desc = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertTrue(desc.getAssertions().contains(assertion));
        assertEquals(1, desc.getAssertionValues(assertion).size());
        return desc.getAssertionValues(assertion).get(0);
    }

    @Test
    public void buildAxiomsExtractsPlainIdentifiersAttributeValuesAsNamedResources() throws Exception {
        final URI uri = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationValue");
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(
                mocks.forOwlClassN().annotationUriAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(PK.toString());
        n.setAnnotationUri(uri);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(n, builder);

        final Value<?> val = getValueForAssertion(builder, annotationWithUriForN());
        assertTrue(val.getValue() instanceof NamedResource);
        assertEquals(uri, ((NamedResource) val.getValue()).getIdentifier());
    }

    @Test
    public void buildAxiomsUsesNullValueWhenExtractedFieldValueIsNull() throws Exception {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(
                mocks.forOwlClassN().annotationUriAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(PK.toString());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(n, builder);

        final Value<?> val = getValueForAssertion(builder, annotationWithUriForN());
        assertSame(Value.nullValue(), val);
    }

    @Test
    public void buildAxiomsSetsLanguageTagAccordingToDescriptorLanguage() throws Exception {
        descriptor.setLanguage("en");
        buildAxiomsAndVerifyLanguageTag();
    }

    private void buildAxiomsAndVerifyLanguageTag() throws Exception {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(PK.toString());
        n.setAnnotationProperty("english");

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(n, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final Assertion assertion = valueDescriptor.getAssertions().iterator().next();
        assertTrue(assertion.hasLanguage());
        assertEquals("en", assertion.getLanguage());
    }

    @Test
    public void buildAxiomsSetsLanguageTagAccordingToPUConfigurationWhenItIsNotSpecifiedInDescriptor()
            throws Exception {
        buildAxiomsAndVerifyLanguageTag();
    }
}