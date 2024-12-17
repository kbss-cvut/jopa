/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class EntityDeconstructorTest {

    private static final URI CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");

    private static OWLClassA entityA;
    private static URI strAttAIdentifier;
    private static OWLClassB entityB;
    private static OWLClassE entityE;
    private static OWLClassD entityD;
    private static URI owlClassAAttIdentifier;
    private static OWLClassM entityM;

    private MetamodelMocks mocks;

    @Mock
    private ObjectOntologyMapperImpl oomMock;

    private EntityDeconstructor sut;

    @BeforeAll
    static void setUpBeforeClass() throws Exception {
        entityA = new OWLClassA();
        entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityA"));
        entityA.setStringAttribute("someStringAttribute");
        strAttAIdentifier = URI.create(OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class).iri());
        entityB = new OWLClassB();
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityB"));
        entityB.setStringAttribute("entityBStringAttribute");
        entityE = new OWLClassE();
        entityE.setStringAttribute("entityEStringAttribute");
        entityD = new OWLClassD();
        entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
        entityD.setOwlClassA(entityA);
        owlClassAAttIdentifier = URI.create(OWLClassD.getOwlClassAField().getAnnotation(OWLObjectProperty.class).iri());
        entityM = new OWLClassM();
        entityM.initializeTestValues(true);

    }

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        this.mocks = new MetamodelMocks();
        when(oomMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        when(oomMock.getConfiguration()).thenReturn(new Configuration(Collections.emptyMap()));
        entityA.setTypes(null);
        entityA.setStringAttribute("someStringAttribute");
        entityB.setProperties(null);
        entityE.setUri(null);
        this.sut = new EntityDeconstructor(oomMock);
        sut.setReferenceSavingResolver(new ReferenceSavingResolver(oomMock));
    }

    @Test
    void testMapEntityWithDataProperty() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = sut
                .mapEntityToAxioms(entityA.getUri(), entityA, mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the data property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(containsInstanceClassAssertion(res, OWLClassA.getClassIri()));
        assertTrue(containsDPAssertion(res, OWLClassA.getStrAttField(), entityA.getStringAttribute()));
    }

    @Test
    void testMapEntityWithDataPropertyNullValue() throws Exception {
        final OWLClassA entity = new OWLClassA();
        entity.setUri(entityA.getUri());
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entity.getUri(), entity,
                                                                 mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entity.getUri(), res.getSubject().getIdentifier());
        assertEquals(2, res.getAssertions().size());
        assertTrue(containsInstanceClassAssertion(res, OWLClassA.getClassIri()));
        final List<Value<?>> v = res.getAssertionValues(Assertion.createDataPropertyAssertion(
                strAttAIdentifier, Generators.LANG, false));
        assertEquals(1, v.size());
        assertEquals(Value.nullValue(), v.get(0));
    }

    @Test
    void testMapEntityWithDataPropertyAndTypes() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final Set<String> types = createTypes();
        entityA.setTypes(types);
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entityA.getUri(),
                                                                 entityA, mocks.forOwlClassA().entityType(),
                                                                 aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the data property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, Generators.LANG, false)));
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
    void testMapEntityWithDataPropertyAndTypesPropertyInDifferentContext() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        aDescriptor.addAttributeContext(mocks.forOwlClassA().stringAttribute(), CONTEXT);
        final Set<String> types = createTypes();
        entityA.setTypes(types);
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entityA.getUri(),
                                                                 entityA, mocks.forOwlClassA().entityType(),
                                                                 aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        assertNull(res.getSubjectContext());
        assertEquals(CONTEXT, res.getAssertionContext(Assertion.createDataPropertyAssertion(
                strAttAIdentifier, Generators.LANG, false)));
        assertNull(res.getAssertionContext(Assertion.createClassAssertion(false)));
    }

    @Test
    void testMapEntityWithObjectProperty() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor();
        when(oomMock.isManaged(entityD.getOwlClassA())).thenReturn(true);
        final AxiomValueGatherer builder = sut
                .mapEntityToAxioms(entityD.getUri(), entityD, mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(containsInstanceClassAssertion(res, OWLClassD.getClassIri()));
        assertTrue(
                containsOPAssertion(res, OWLClassD.getOwlClassAField(), NamedResource.create(entityA.getUri())));
    }

    private boolean containsOPAssertion(AxiomValueDescriptor descriptor, Field attributeField, Object value) {
        final URI propertyUri = URI.create(attributeField.getAnnotation(OWLObjectProperty.class).iri());
        final Assertion assertion = Assertion
                .createObjectPropertyAssertion(propertyUri, attributeField.getAnnotation(Inferred.class) != null);
        return containsAssertionValue(descriptor, assertion, value);
    }

    @Test
    void testMapEntityWithObjectPropertyNullValue() throws Exception {
        final OWLClassD entity = new OWLClassD();
        entity.setUri(entityD.getUri());
        final Descriptor dDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entity.getUri(), entity,
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
    void testMapEntityWithObjectPropertyAndContext() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT);
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entityD.getUri(),
                                                                 entityD, mocks.forOwlClassD().entityType(),
                                                                 dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
        assertEquals(CONTEXT, res.getSubjectContext());
        for (Assertion ass : res.getAssertions()) {
            assertEquals(CONTEXT, res.getAssertionContext(ass));
        }
    }

    @Test
    void testMapEntityWithProperties() throws Exception {
        final Descriptor bDescriptor = new EntityDescriptor();
        final Map<String, Set<String>> props = Generators.generateStringProperties();
        entityB.setProperties(props);
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entityB.getUri(),
                                                                 entityB, mocks.forOwlClassB().entityType(),
                                                                 bDescriptor);
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
    void testMapEntityWithNullProperties() throws Exception {
        final Descriptor bDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entityB.getUri(),
                                                                 entityB, mocks.forOwlClassB().entityType(),
                                                                 bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        // Class assertion, data property assertion
        assertEquals(2, res.getAssertions().size());
    }

    @Test
    void testMapEntityWithPropertiesMultipleValuesPerProperty() throws Exception {
        final Map<String, Set<String>> props = createProperties();
        entityB.setProperties(props);
        final Descriptor bDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = sut.mapEntityToAxioms(entityB.getUri(),
                                                                 entityB, mocks.forOwlClassB().entityType(),
                                                                 bDescriptor);
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
    void mapsEntityDataPropertyFieldToAxiomDescriptor() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = sut.mapFieldToAxioms(entityA.getUri(), entityA,
                                                                mocks.forOwlClassA().stringAttribute(),
                                                                mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(containsDPAssertion(res, OWLClassA.getStrAttField(), entityA.getStringAttribute()));
    }

    @Test
    void mapsEntityDataPropertyWithNullValueToAxiomDescriptor() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        entityA.setStringAttribute(null);
        final AxiomValueGatherer builder = sut.mapFieldToAxioms(entityA.getUri(), entityA,
                                                                mocks.forOwlClassA().stringAttribute(),
                                                                mocks.forOwlClassA().entityType(), aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier,
                                                      mocks.forOwlClassA().stringAttribute().getLanguage(),
                                                      mocks.forOwlClassA().stringAttribute().isInferred())));
        final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
        assertEquals(1, val.size());
        assertEquals(Value.nullValue(), val.get(0));
        assertNull(val.get(0).getValue());
    }

    @Test
    void mapsEntityObjectPropertyValueInContextToAxiomDescriptor() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor(false);
        dDescriptor.addAttributeContext(mocks.forOwlClassD().owlClassAAtt(), CONTEXT);
        when(oomMock.isManaged(entityD.getOwlClassA())).thenReturn(true);
        final AxiomValueGatherer builder = sut.mapFieldToAxioms(entityD.getUri(), entityD,
                                                                mocks.forOwlClassD().owlClassAAtt(),
                                                                mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        final Assertion ass = Assertion.createObjectPropertyAssertion(owlClassAAttIdentifier,
                                                                      mocks.forOwlClassD().owlClassAAtt().isInferred());
        assertEquals(CONTEXT, res.getAssertionContext(ass));
        assertTrue(
                containsOPAssertion(res, OWLClassD.getOwlClassAField(), NamedResource.create(entityA.getUri())));
    }

    @Test
    void mapsEntityWithStringKeyAndBasicDataAttributes() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomValueGatherer builder =
                sut
                        .mapEntityToAxioms(URI.create(entityM.getKey()), entityM, mocks.forOwlClassM().entityType(),
                                           desc);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertTrue(containsInstanceClassAssertion(res, OWLClassM.getClassIri()));
        assertTrue(containsDPAssertion(res, OWLClassM.getBooleanAttributeField(), entityM.getBooleanAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getIntAttributeField(), entityM.getIntAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getDoubleAttributeField(), entityM.getDoubleAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getLongAttributeField(), entityM.getLongAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getDateAttributeField(), entityM.getDateAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getCharacterAttributeField(), entityM.getCharacterAttribute().toString()));
    }

    private boolean containsInstanceClassAssertion(AxiomValueDescriptor descriptor, String classIri) {
        final List<Value<?>> values = descriptor.getAssertionValues(Assertion.createClassAssertion(false));
        assertEquals(1, values.size());
        return values.get(0).getValue().toString().equals(classIri);
    }

    private boolean containsDPAssertion(AxiomValueDescriptor descriptor, Field attributeField, Object value) {
        final OWLDataProperty annotation = attributeField.getAnnotation(OWLDataProperty.class);
        final URI propertyUri = URI.create(annotation.iri());
        final Assertion assertion = Assertion
                .createDataPropertyAssertion(propertyUri, Generators.LANG, attributeField.getAnnotation(
                        Inferred.class) != null);
        return containsAssertionValue(descriptor, assertion, value);
    }

    private boolean containsAssertionValue(AxiomValueDescriptor descriptor, Assertion assertion, Object value) {
        final List<Value<?>> values = descriptor.getAssertionValues(assertion);
        assertFalse(values.isEmpty());
        assertEquals(1, values.size());
        for (Value<?> val : values) {
            if (val.getValue().equals(value)) {
                return true;
            }
        }
        return false;
    }

    @Test
    void mapEntityToAxiomsIncludesAttributesInheritedFromMappedSuperclass() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final OWLClassQ q = initQ();

        final AxiomValueGatherer builder = sut
                .mapEntityToAxioms(q.getUri(), q, mocks.forOwlClassQ().entityType(), desc);
        final AxiomValueDescriptor valueDescriptor = getAxiomValueDescriptor(builder);
        assertTrue(containsInstanceClassAssertion(valueDescriptor, OWLClassQ.getClassIri()));
        assertTrue(containsDPAssertion(valueDescriptor, OWLClassQ.getStringAttributeField(), q.getStringAttribute()
        ));
        assertTrue(containsDPAssertion(valueDescriptor, OWLClassQ.getParentStringField(), q.getParentString()));
        assertTrue(containsAPAssertion(valueDescriptor, OWLClassQ.getLabelField(), q.getLabel()));
        assertTrue(containsOPAssertion(valueDescriptor, OWLClassQ.getOwlClassAField(),
                                       NamedResource.create(entityA.getUri())));
    }

    private OWLClassQ initQ() {
        final OWLClassQ q = new OWLClassQ();
        q.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/test#entityQ"));
        q.setStringAttribute("stringAttribute");
        q.setParentString("parentStringAttribute");
        q.setLabel("label");
        q.setOwlClassA(entityA);
        when(oomMock.isManaged(entityA)).thenReturn(true);
        return q;
    }

    private boolean containsAPAssertion(AxiomValueDescriptor descriptor, Field attributeField, Object value) {
        final URI propertyUri = URI.create(attributeField.getAnnotation(OWLAnnotationProperty.class).iri());
        final Assertion assertion = Assertion
                .createAnnotationPropertyAssertion(propertyUri, Generators.LANG,
                                                   attributeField.getAnnotation(Inferred.class) != null);
        return containsAssertionValue(descriptor, assertion, value);
    }

    private static AxiomValueDescriptor getAxiomValueDescriptor(AxiomValueGatherer builder)
            throws Exception {
        return OOMTestUtils.getAxiomValueDescriptor(builder);
    }

    @Test
    void mapEntityToAxiomsSupportsPluralAnnotationProperty() throws Exception {
        final Descriptor descriptor = new EntityDescriptor();
        final OWLClassN n = new OWLClassN();
        n.setId(Generators.createIndividualIdentifier().toString());
        n.setPluralAnnotation(
                IntStream.range(0, 5).mapToObj(i -> Generators.createIndividualIdentifier().toString()).collect(
                        Collectors.toSet()));
        final AxiomValueGatherer builder = sut
                .mapEntityToAxioms(URI.create(n.getId()), n, mocks.forOwlClassN().entityType(), descriptor);

        final AxiomValueDescriptor valueDescriptor = getAxiomValueDescriptor(builder);
        final Assertion assertion =
                Assertion.createAnnotationPropertyAssertion(URI.create(DC.Terms.SOURCE), Generators.LANG, false);
        assertTrue(valueDescriptor.getAssertions().contains(assertion));
        assertAll(() -> assertEquals(n.getPluralAnnotation().size(),
                                     valueDescriptor.getAssertionValues(assertion).size()),
                  () -> {
                      final List<Value<String>> values =
                              n.getPluralAnnotation().stream().map(Value::new).toList();
                      assertTrue(valueDescriptor.getAssertionValues(assertion).containsAll(values));
                  });
    }

    @Test
    void mapsEntityObjectPropertyAssertionToSubjectContextWhenAssertionsInSubjectContextAreConfigured()
            throws Exception {
        final URI subjectContext = Generators.createIndividualIdentifier();
        final Descriptor dDescriptor = new EntityDescriptor(subjectContext);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT);
        dDescriptor.addAttributeDescriptor(mocks.forOwlClassD().owlClassAAtt(), aDescriptor);
        when(oomMock.isManaged(entityD.getOwlClassA())).thenReturn(true);
        final AxiomValueGatherer builder = sut.mapFieldToAxioms(entityD.getUri(), entityD,
                                                                mocks.forOwlClassD().owlClassAAtt(),
                                                                mocks.forOwlClassD().entityType(), dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        final Assertion ass = Assertion.createObjectPropertyAssertion(owlClassAAttIdentifier,
                                                                      mocks.forOwlClassD().owlClassAAtt().isInferred());
        assertEquals(subjectContext, res.getAssertionContext(ass));
    }

    @Test
    void mapFieldToAxiomsMapsSimpleLiteralFieldToAxiomWithAssertionWithoutLanguageTag() throws Exception {
        final String value = "test";
        entityM.setSimpleLiteral(value);
        final AxiomValueGatherer builder = sut
                .mapFieldToAxioms(URI.create(entityM.getKey()), entityM, mocks.forOwlClassM().simpleLiteralAttribute(),
                                  mocks.forOwlClassM().entityType(), new EntityDescriptor());
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        final Assertion a = res.getAssertions().iterator().next();
        assertEquals(Vocabulary.p_m_simpleLiteral, a.getIdentifier().toString());
        assertFalse(a.hasLanguage());
        assertEquals(value, res.getAssertionValues(a).get(0).getValue());
    }
}
