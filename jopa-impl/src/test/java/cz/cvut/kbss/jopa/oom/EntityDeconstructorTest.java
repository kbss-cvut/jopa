/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class EntityDeconstructorTest {

    private static final URI CONTEXT = URI
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");

    private static OWLClassA entityA;
    private static URI strAttAIdentifier;
    private static OWLClassB entityB;
    private static OWLClassE entityE;
    private static OWLClassD entityD;
    private static URI owlClassAAttIdentifier;
    private static OWLClassK entityK;
    private static OWLClassM entityM;

    private MetamodelMocks mocks;

    @Mock
    private ObjectOntologyMapperImpl oomMock;

    @Mock
    private CascadeResolver cascadeResolverMock;

    private EntityDeconstructor entityBreaker;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        entityA = new OWLClassA();
        entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityA"));
        entityA.setStringAttribute("someStringAttribute");
        strAttAIdentifier = URI.create(OWLClassA.getStrAttField()
                                                .getAnnotation(OWLDataProperty.class).iri());
        entityB = new OWLClassB();
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityB"));
        entityB.setStringAttribute("entityBStringAttribute");
        entityE = new OWLClassE();
        entityE.setStringAttribute("entityEStringAttribute");
        entityD = new OWLClassD();
        entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
        entityD.setOwlClassA(entityA);
        owlClassAAttIdentifier = URI.create(OWLClassD.getOwlClassAField()
                                                     .getAnnotation(OWLObjectProperty.class).iri());
        entityK = new OWLClassK();
        entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
        entityK.setOwlClassE(entityE);
        entityM = new OWLClassM();
        entityM.initializeTestValues(true);

    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.mocks = new MetamodelMocks();
        when(oomMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        entityA.setTypes(null);
        entityA.setStringAttribute("someStringAttribute");
        entityB.setProperties(null);
        entityE.setUri(null);
        this.entityBreaker = new EntityDeconstructor(oomMock);
        entityBreaker.setCascadeResolver(cascadeResolverMock);
    }

    @Test
    public void testMapEntityWithDataProperty() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker
                .mapEntityToAxioms(entityA.getUri(), entityA, mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the data property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
    }

    @Test
    public void testMapEntityWithDataPropertyNullValue() throws Exception {
        final OWLClassA entity = new OWLClassA();
        entity.setUri(entityA.getUri());
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entity.getUri(), entity,
                mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entity.getUri(), res.getSubject().getIdentifier());
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        final List<Value<?>> v = res.getAssertionValues(Assertion.createDataPropertyAssertion(
                strAttAIdentifier, false));
        assertEquals(1, v.size());
        assertEquals(Value.nullValue(), v.get(0));
    }

    @Test
    public void testMapEntityWithDataPropertyAndTypes() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final Set<String> types = createTypes();
        entityA.setTypes(types);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
                entityA, mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the data property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
        // The entity class + the declared types
        res.getAssertions().stream().filter(a -> a.getType() == AssertionType.CLASS).forEach(a -> {
            final List<Value<?>> cls = res.getAssertionValues(a);
            // The entity class + the declared types
            assertEquals(1, cls.size());
            assertEquals(OWLClassA.getClassIri(), cls.get(0).stringValue());
        });
        final Set<URI> typesRes = OOMTestUtils.getTypesToAdd(builder);
        assertEquals(types.size(), typesRes.size());
        for (URI u : typesRes) {
            assertTrue(types.contains(u.toString()));
        }
    }

    private Set<String> createTypes() {
        final Set<String> types = new HashSet<>();
        types.add("http://krizik.felk.cvut.cz/ontologies/entityX");
        types.add("http://krizik.felk.cvut.cz/ontologies/entityY");
        return types;
    }

    @Test
    public void testMapEntityWithDataPropertyAndTypesPropertyInDifferentContext() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        aDescriptor.addAttributeContext(OWLClassA.getStrAttField(), CONTEXT);
        final Set<String> types = createTypes();
        entityA.setTypes(types);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
                entityA, mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        assertNull(res.getSubjectContext());
        assertEquals(CONTEXT, res.getAssertionContext(Assertion.createDataPropertyAssertion(
                strAttAIdentifier, false)));
        assertNull(res.getAssertionContext(Assertion.createClassAssertion(false)));
    }

    @Test
    public void testMapEntityWithObjectProperty() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityD.getUri(),
                entityD, mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        for (Assertion a : res.getAssertions()) {
            final List<Value<?>> vals = res.getAssertionValues(a);
            if (a.getType() == AssertionType.CLASS) {
                assertEquals(1, vals.size());
                assertEquals(URI.create(OWLClassD.getClassIri()), vals.get(0).getValue());
            } else {
                assertTrue(AssertionType.OBJECT_PROPERTY == a.getType());
                assertEquals(1, vals.size());
                assertEquals(NamedResource.create(entityA.getUri()), vals.get(0).getValue());
            }
        }
    }

    @Test
    public void testMapEntityWithObjectPropertyNullValue() throws Exception {
        final OWLClassD entity = new OWLClassD();
        entity.setUri(entityD.getUri());
        final Descriptor dDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entity.getUri(), entity,
                mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entity.getUri(), res.getSubject().getIdentifier());
        // Only the class assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        final List<Value<?>> v = res.getAssertionValues(Assertion.createObjectPropertyAssertion(
                owlClassAAttIdentifier, false));
        assertEquals(1, v.size());
        assertEquals(Value.nullValue(), v.get(0));
    }

    @Test
    public void testMapEntityWithObjectPropertyAndContext() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityD.getUri(),
                entityD, mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
        assertEquals(CONTEXT, res.getSubjectContext());
        for (Assertion ass : res.getAssertions()) {
            assertEquals(CONTEXT, res.getAssertionContext(ass));
        }
    }

    @Test
    public void testMapEntityWithProperties() throws Exception {
        final Descriptor bDescriptor = new EntityDescriptor();
        final Map<String, Set<String>> props = Generators.generateStringProperties();
        entityB.setProperties(props);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
                entityB, mocks.forOwlClassB().entityType(), bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        // Class assertion, data property assertion and the properties
        // assertions
        assertEquals(2, res.getAssertions().size());
        verifyPropertiesForSave(props, builder);
    }

    private void verifyPropertiesForSave(Map<String, Set<String>> props, AxiomValueGatherer builder) throws Exception {
        final Map<Assertion, Set<Value<?>>> resultProperties = OOMTestUtils.getPropertiesToAdd(builder);
        for (Assertion a : resultProperties.keySet()) {
            assertTrue(props.containsKey(a.getIdentifier().toString()));
            final Set<String> propValues = props.get(a.getIdentifier().toString());
            for (Value<?> v : resultProperties.get(a)) {
                assertTrue(propValues.contains(v.stringValue()));
            }
        }
    }

    @Test
    public void testMapEntityWithNullProperties() throws Exception {
        final Descriptor bDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
                entityB, mocks.forOwlClassB().entityType(), bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        // Class assertion, data property assertion
        assertEquals(2, res.getAssertions().size());
    }

    @Test
    public void testMapEntityWithPropertiesMultipleValuesPerProperty() throws Exception {
        final Map<String, Set<String>> props = createProperties();
        entityB.setProperties(props);
        final Descriptor bDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
                entityB, mocks.forOwlClassB().entityType(), bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        assertEquals(2, res.getAssertions().size());
        verifyPropertiesForSave(props, builder);
    }

    private Map<String, Set<String>> createProperties() {
        final Map<String, Set<String>> map = new HashMap<>();
        for (int i = 0; i < 5; i++) {
            final String key = "http://krizik.felk.cvut.cz/ontologies/jopa/someProperty" + i;
            final Set<String> vals = new HashSet<>();
            for (int j = 0; j < 5; j++) {
                vals.add("dataValue" + j);
            }
            map.put(key, vals);
        }
        return map;
    }

    @Test
    public void testMapEntityWithObjectPropertyWithGeneratedIdentifier() throws Exception {
        final URI eUri = URI.create("http://eUri");
        when(oomMock.generateIdentifier(mocks.forOwlClassE().entityType())).thenReturn(eUri);
        when(oomMock.getEntityType(OWLClassE.class)).thenReturn(mocks.forOwlClassE().entityType());
        final Descriptor eDescriptor = new EntityDescriptor(CONTEXT);
        final Descriptor kDescriptor = new EntityDescriptor();
        kDescriptor.addAttributeDescriptor(OWLClassK.getOwlClassEField(), eDescriptor);
        assertNull(entityE.getUri());
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityK.getUri(),
                entityK, mocks.forOwlClassK().entityType(), kDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        verify(oomMock).generateIdentifier(mocks.forOwlClassE().entityType());
        assertEquals(eUri, entityE.getUri());
    }

    @Test
    public void mapsEntityDataPropertyFieldToAxiomDescriptor() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityA.getUri(), entityA,
                OWLClassA.getStrAttField(), mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier,
                        mocks.forOwlClassA().stringAttribute().isInferred())));
        final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
        assertEquals(1, val.size());
        assertEquals(entityA.getStringAttribute(), val.get(0).getValue());
    }

    @Test
    public void mapsEntityDataPropertyWithNullValueToAxiomDescriptor() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        entityA.setStringAttribute(null);
        final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityA.getUri(), entityA,
                OWLClassA.getStrAttField(), mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier,
                        mocks.forOwlClassA().stringAttribute().isInferred())));
        final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
        assertEquals(1, val.size());
        assertEquals(Value.nullValue(), val.get(0));
        assertNull(val.get(0).getValue());
    }

    @Test
    public void mapsEntityObjectPropertyValueInContextToAxiomDescriptor() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor();
        dDescriptor.addAttributeContext(OWLClassD.getOwlClassAField(), CONTEXT);
        final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityD.getUri(), entityD,
                OWLClassD.getOwlClassAField(), mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createObjectPropertyAssertion(owlClassAAttIdentifier,
                        mocks.forOwlClassD().owlClassAAtt().isInferred())));
        final Assertion ass = res.getAssertions().iterator().next();
        final List<Value<?>> val = res.getAssertionValues(ass);
        assertEquals(1, val.size());
        assertEquals(CONTEXT, res.getAssertionContext(ass));
        assertEquals(NamedResource.create(entityA.getUri()), val.get(0).getValue());
    }

    @Test
    public void mapsEntityWithStringKeyAndBasicDataAttributes() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomValueGatherer builder =
                entityBreaker
                        .mapEntityToAxioms(URI.create(entityM.getKey()), entityM, mocks.forOwlClassM().entityType(),
                                desc);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertTrue(containsInstanceAssertion(res));
        assertTrue(containsDPAssertion(res, OWLClassM.getBooleanAttributeField(), entityM.getBooleanAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getIntAttributeField(), entityM.getIntAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getDoubleAttributeField(), entityM.getDoubleAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getLongAttributeField(), entityM.getLongAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getDateAttributeField(), entityM.getDateAttribute()));
    }

    private boolean containsInstanceAssertion(AxiomValueDescriptor descriptor) throws Exception {
        final List<Value<?>> values = descriptor.getAssertionValues(Assertion.createClassAssertion(false));
        assertEquals(1, values.size());
        return values.get(0).getValue().toString().equals(OWLClassM.getClassIri());
    }

    private boolean containsDPAssertion(AxiomValueDescriptor descriptor, Field attributeField, Object value) {
        OWLDataProperty annotation = attributeField.getAnnotation(OWLDataProperty.class);
        final URI propertyUri = URI.create(annotation.iri());
        final Assertion assertion = Assertion.createDataPropertyAssertion(propertyUri, attributeField.getAnnotation(
                Inferred.class) != null);
        final List<Value<?>> values = descriptor.getAssertionValues(assertion);
        for (Value<?> val : values) {
            if (val.getValue().equals(value)) {
                return true;
            }
        }
        return false;
    }

    private static AxiomValueDescriptor getAxiomValueDescriptor(AxiomValueGatherer builder)
            throws Exception {
        return OOMTestUtils.getAxiomValueDescriptor(builder);
    }
}
