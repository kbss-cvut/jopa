/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class SingularDataPropertyStrategyTest {

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
    public void buildAxiomsSetsLanguageTagAccordingToDescriptorLanguage() throws Exception {
        descriptor.setLanguage("en");
        buildAxiomsAndVerifyLanguageTag();
    }

    private void buildAxiomsAndVerifyLanguageTag() throws Exception {
        final SingularDataPropertyStrategy<OWLClassA> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassA().entityType(), mocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        final OWLClassA a = new OWLClassA();
        a.setUri(PK);
        a.setStringAttribute("english");

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(a, builder);
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

    @Test
    public void buildAxiomsTransformsLocalDateToJavaUtilDate() throws Exception {
        final SingularDataPropertyStrategy<OWLClassT> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassT().entityType(), mocks.forOwlClassT().tLocalDateAtt(), descriptor, mapperMock);
        final OWLClassT t = new OWLClassT();
        t.setUri(PK);
        t.setLocalDate(LocalDate.now());

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(t, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor
                .getAssertionValues(valueDescriptor.getAssertions().iterator().next());
        assertEquals(1, values.size());
        final Object value = values.get(0).getValue();
        assertTrue(value instanceof Date);
    }

    @Test
    public void buildAxiomsTransformsLocalDateTimeToJavaUtilDate() throws Exception {
        final SingularDataPropertyStrategy<OWLClassT> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassT().entityType(), mocks.forOwlClassT().tLocalDateTimeAtt(), descriptor, mapperMock);
        final OWLClassT t = new OWLClassT();
        t.setUri(PK);
        t.setLocalDateTime(LocalDateTime.now());

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(t, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor
                .getAssertionValues(valueDescriptor.getAssertions().iterator().next());
        assertEquals(1, values.size());
        final Object value = values.get(0).getValue();
        assertTrue(value instanceof Date);
    }

    @Test
    public void buildInstanceFieldValueTransformsJavaUtilDateToLocalDate() {
        final SingularDataPropertyStrategy<OWLClassT> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassT().entityType(), mocks.forOwlClassT().tLocalDateAtt(), descriptor, mapperMock);
        final OWLClassT t = new OWLClassT();
        t.setUri(PK);

        final Axiom<Date> axiom = new AxiomImpl<>(NamedResource.create(PK), strategy.createAssertion(),
                new Value<>(new Date()));
        strategy.addValueFromAxiom(axiom);
        strategy.buildInstanceFieldValue(t);
        assertNotNull(t.getLocalDate());
    }

    @Test
    public void buildInstanceFieldValueTransformsJavaUtilDateToLocalDateTime() {
        final SingularDataPropertyStrategy<OWLClassT> strategy = new SingularDataPropertyStrategy<>(
                mocks.forOwlClassT().entityType(), mocks.forOwlClassT().tLocalDateTimeAtt(), descriptor, mapperMock);
        final OWLClassT t = new OWLClassT();
        t.setUri(PK);

        final Axiom<Date> axiom = new AxiomImpl<>(NamedResource.create(PK), strategy.createAssertion(),
                new Value<>(new Date()));
        strategy.addValueFromAxiom(axiom);
        strategy.buildInstanceFieldValue(t);
        assertNotNull(t.getLocalDateTime());
    }

    @Test
    public void buildInstanceFieldConvertsRepositoryValueToEnum() {
        final SingularDataPropertyStrategy<OWLClassM> sut =
                new SingularDataPropertyStrategy<>(mocks.forOwlClassM().entityType(),
                        mocks.forOwlClassM().enumAttribute(), descriptor, mapperMock);
        final OWLClassM m = new OWLClassM();
        m.setKey(PK.toString());

        final Axiom<String> axiom = new AxiomImpl<>(NamedResource.create(PK), sut.createAssertion(),
                new Value<>(OWLClassM.Severity.MEDIUM.toString()));
        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(m);
        assertEquals(OWLClassM.Severity.MEDIUM, m.getEnumAttribute());
    }
}