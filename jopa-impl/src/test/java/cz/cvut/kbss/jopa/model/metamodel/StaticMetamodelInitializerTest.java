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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA_;
import cz.cvut.kbss.jopa.environment.OWLClassB_;
import cz.cvut.kbss.jopa.environment.OWLClassC_;
import cz.cvut.kbss.jopa.environment.OWLClassQ_;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass_;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exception.StaticMetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class StaticMetamodelInitializerTest {

    @Mock
    private Metamodel metamodel;

    private MetamodelMocks metamodelMocks;

    @InjectMocks
    private StaticMetamodelInitializer sut;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
    }

    @Test
    void initializeStaticMetamodelInitializesFieldsOfSimpleEntityStaticMetamodel() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassA().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassA().identifier(), OWLClassA_.uri);
        assertEquals(metamodelMocks.forOwlClassA().stringAttribute(), OWLClassA_.stringAttribute);
        assertEquals(metamodelMocks.forOwlClassA().typesSpec(), OWLClassA_.types);
    }

    @Test
    void initializeStaticMetamodelPopulatesEntityClassIriField() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassA().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassA().entityType().getIRI(), OWLClassA_.entityClassIRI);
    }

    @Test
    void initializeStaticMetamodelInitializesPropertiesFieldInStaticMetamodelClass() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassB().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassB().identifier(), OWLClassB_.uri);
        assertEquals(metamodelMocks.forOwlClassB().stringAttribute(), OWLClassB_.stringAttribute);
        assertEquals(metamodelMocks.forOwlClassB().propertiesSpec(), OWLClassB_.properties);
    }

    @Test
    void initializeStaticMetamodelInitializesPluralAttributesInStaticMetamodelClass() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassC().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassC().identifier(), OWLClassC_.uri);
        assertEquals(metamodelMocks.forOwlClassC().referencedListAtt(), OWLClassC_.referencedList);
        assertEquals(metamodelMocks.forOwlClassC().simpleListAtt(), OWLClassC_.simpleList);
    }

    @Test
    void initializeStaticMetamodelThrowsStaticMetamodelInitializationExceptionWhenStaticMetamodelFieldHasNoMetamodelCounterpart()
            throws Exception {
        final EntityType<NoMatching> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(NoMatching.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        when(et.getDeclaredAttribute(any())).thenThrow(IllegalArgumentException.class);
        when(et.getJavaType()).thenReturn(NoMatching.class);
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        assertThrows(StaticMetamodelInitializationException.class, () -> sut.initializeStaticMetamodel());
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "NoMatching")
    public static class NoMatching {

        @Id
        private URI uri;

        @Types
        private Set<String> types;

        @Properties
        private Map<String, Set<String>> properties;
    }

    @StaticMetamodel(NoMatching.class)
    public static class NoMatching_ {
        public static volatile Identifier<NoMatching, URI> uri;

        public static volatile Attribute<NoMatching, String> unknown;
    }

    @Test
    void initializeStaticMetamodelIgnoresClassWithoutMatchingStaticMetamodelAnnotation() throws Exception {
        final EntityType<NoStaticMetamodelAnnotation> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(NoStaticMetamodelAnnotation.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        when(et.getDeclaredAttribute(any())).thenThrow(IllegalArgumentException.class);
        when(et.getJavaType()).thenReturn(NoStaticMetamodelAnnotation.class);
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        assertNull(NoStaticMetamodelAnnotation_.uri);
        sut.initializeStaticMetamodel();
        assertNull(NoStaticMetamodelAnnotation_.uri);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "NoStaticMetamodelAnnotation")
    public static class NoStaticMetamodelAnnotation {
        @Id
        private URI uri;
    }

    public static class NoStaticMetamodelAnnotation_ {
        public static volatile Identifier<NoMatching, URI> uri;
    }

    @Test
    void initializeStaticMetamodelIgnoresEntityIdentifierWhenItIsInheritedFromSuperclass() throws Exception {
        final EntityType<WithInheritedIdentifier> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(NoMatching.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        when(et.getDeclaredAttribute(any())).thenThrow(IllegalArgumentException.class);
        when(et.getJavaType()).thenReturn(WithInheritedIdentifier.class);
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        assertThrows(StaticMetamodelInitializationException.class, () -> sut.initializeStaticMetamodel());
        assertNull(WithInheritedIdentifier_.uri);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithInheritedIdentifier")
    public static class WithInheritedIdentifier {

    }

    @StaticMetamodel(WithInheritedIdentifier.class)
    public static class WithInheritedIdentifier_ {
        public static volatile Identifier<WithInheritedIdentifier, URI> uri;
    }

    @Test
    void initializeStaticMetamodelIgnoresTypesSpecificationWhenItIsInheritedFromSuperclass() throws Exception {
        final EntityType<WithInheritedTypes> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(NoMatching.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final TypesSpecification types = mock(TypesSpecification.class);
        when(types.getJavaField()).thenReturn(NoMatching.class.getDeclaredField("types"));
        when(et.getTypes()).thenReturn(types);
        when(et.getDeclaredAttribute(any())).thenThrow(IllegalArgumentException.class);
        when(et.getJavaType()).thenReturn(WithInheritedTypes.class);
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        assertThrows(StaticMetamodelInitializationException.class, () -> sut.initializeStaticMetamodel());
        assertNull(WithInheritedTypes_.types);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithInheritedTypes")
    public static class WithInheritedTypes {

    }

    @StaticMetamodel(WithInheritedTypes.class)
    public static class WithInheritedTypes_ {
        public static volatile TypesSpecification<WithInheritedTypes, String> types;
    }

    @Test
    void initializeStaticMetamodelIgnoresPropertiesSpecificationWhenItIsInheritedFromSuperclass() throws Exception {
        final EntityType<WithInheritedProperties> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(NoMatching.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        final PropertiesSpecification types = mock(PropertiesSpecification.class);
        when(types.getJavaField()).thenReturn(NoMatching.class.getDeclaredField("properties"));
        when(et.getProperties()).thenReturn(types);
        when(et.getDeclaredAttribute(any())).thenThrow(IllegalArgumentException.class);
        when(et.getJavaType()).thenReturn(WithInheritedProperties.class);
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        assertThrows(StaticMetamodelInitializationException.class, () -> sut.initializeStaticMetamodel());
        assertNull(WithInheritedProperties_.properties);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithInheritedProperties")
    public static class WithInheritedProperties {

    }

    @StaticMetamodel(WithInheritedProperties.class)
    public static class WithInheritedProperties_ {
        public static volatile PropertiesSpecification<WithInheritedProperties, Set<String>, String, String> properties;
    }

    @Test
    void initializeStaticMetamodelInitializesMetamodelForMappedSuperclasses() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassQ().entityType()));
        doReturn(new HashSet<>(Arrays.asList(metamodelMocks.forOwlClassQ().entityType(),
                metamodelMocks.forOwlClassQ().entityType().getSupertypes().iterator().next()))).when(metamodel)
                                                                                               .getManagedTypes();
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassQ().identifier(), QMappedSuperclass_.uri);
        assertEquals(metamodelMocks.forOwlClassQ().qLabelAtt(), QMappedSuperclass_.label);
        assertEquals(metamodelMocks.forOwlClassQ().qParentStringAtt(), QMappedSuperclass_.parentString);
        assertEquals(metamodelMocks.forOwlClassQ().qOwlClassAAtt(), QMappedSuperclass_.owlClassA);
        assertEquals(metamodelMocks.forOwlClassQ().qStringAtt(), OWLClassQ_.stringAttribute);
    }

    @Test
    void initializeStaticMetamodelChecksThatModelSubclassHasDeclaredSuperclassInStaticMetamodelAsWell()
            throws Exception {
        final IdentifiableEntityType<SubClass> et = mock(IdentifiableEntityType.class);
        final SingularAttributeImpl labelAtt = mock(SingularAttributeImpl.class);
        when(labelAtt.getJavaField()).thenReturn(SubClass.class.getDeclaredField("label"));
        when(labelAtt.getDeclaringType()).thenReturn(et);
        when(et.getDeclaredAttribute("label")).thenReturn(labelAtt);
        when(et.getJavaType()).thenReturn(SubClass.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(SuperClass.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);

        final MappedSuperclassTypeImpl<SuperClass> superType = mock(MappedSuperclassTypeImpl.class);

        when(superType.getIdentifier()).thenReturn(id);
        when(superType.getJavaType()).thenReturn(SuperClass.class);
        when(et.getSupertypes()).thenReturn(Collections.singleton(superType));
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        doReturn(new HashSet<>(Arrays.asList(et, superType))).when(metamodel).getManagedTypes();

        assertThrows(StaticMetamodelInitializationException.class, () -> sut.initializeStaticMetamodel());
    }

    @MappedSuperclass
    public static class SuperClass {
        @Id
        private URI uri;
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "SubClass")
    public static class SubClass extends SuperClass {

        @OWLAnnotationProperty(iri = RDFS.LABEL)
        private String label;
    }

    @StaticMetamodel(SubClass.class)
    public static class SubClass_ {
        public static volatile SingularAttribute<SubClass, String> label;
    }

    @Test
    void initializeStaticMetamodelSkipsFieldsWhichAreNotPublicStaticVolatile() throws Exception {
        final EntityType<SimpleClass> et = mock(EntityType.class);
        final Identifier id = mock(Identifier.class);
        when(id.getJavaField()).thenReturn(SimpleClass.class.getDeclaredField("uri"));
        when(et.getIdentifier()).thenReturn(id);
        when(et.getJavaType()).thenReturn(SimpleClass.class);
        when(et.getDeclaredAttribute(anyString())).thenThrow(IllegalArgumentException.class);
        when(metamodel.getEntities()).thenReturn(Collections.singleton(et));
        sut.initializeStaticMetamodel();
        assertEquals(id, SimpleClass_.uri);
        assertNull(SimpleClass_.test);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "SimpleClass")
    public static class SimpleClass {

        @Id
        private URI uri;
    }

    @StaticMetamodel(SimpleClass.class)
    public static class SimpleClass_ {

        public static volatile Identifier<SimpleClass, URI> uri;

        private static String test;
    }

    @Test
    void initializeStaticMetamodelInitializesPropertyIriFields() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassA().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassA().stringAttribute().getIRI(), OWLClassA_.stringAttributePropertyIRI);
    }
}
