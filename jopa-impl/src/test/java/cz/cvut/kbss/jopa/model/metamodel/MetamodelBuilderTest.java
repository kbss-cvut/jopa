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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.OWLClassV;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.Namespace;
import cz.cvut.kbss.jopa.model.annotations.Namespaces;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.RDFCollection;
import cz.cvut.kbss.jopa.model.annotations.RDFContainer;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.annotations.Sparql;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import cz.cvut.kbss.jopa.oom.converter.ObjectConverter;
import cz.cvut.kbss.jopa.oom.converter.ObjectOneOfEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.ToIntegerConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.LocalDateTimeConverter;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.CoreMatchers.containsStringIgnoringCase;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("unused")
class MetamodelBuilderTest {

    @Mock
    private PersistenceUnitClassFinder finderMock;

    private final MetamodelBuilder builder = new MetamodelBuilder(new Configuration());

    @Test
    void buildsMetamodelOfEntityWithSingleNamespaceDeclaredOnClass() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(EntityWithNamespace.class));
        builder.buildMetamodel(finderMock);
        final EntityType<EntityWithNamespace> result =
                (EntityType<EntityWithNamespace>) builder.getEntityClass(EntityWithNamespace.class);
        assertEquals(Vocabulary.CLASS_BASE + "EntityWithNamespace", result.getIRI().toString());
    }

    @TestLocal
    @Namespace(prefix = "class", namespace = Vocabulary.CLASS_BASE)
    @OWLClass(iri = "class:EntityWithNamespace")
    public static class EntityWithNamespace {
        @Id
        private URI uri;
    }

    @Test
    void buildsMetamodelOfEntityWithNamespacesDeclaredOnClass() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(EntityWithNamespace.class));
        builder.buildMetamodel(finderMock);
        final EntityType<EntityWithNamespaces> result =
                (EntityType<EntityWithNamespaces>) builder.getEntityClass(EntityWithNamespaces.class);
        assertEquals(Vocabulary.CLASS_BASE + "EntityWithNamespaces", result.getIRI().toString());
    }

    @TestLocal
    @Namespaces({@Namespace(prefix = "class", namespace = Vocabulary.CLASS_BASE)})
    @OWLClass(iri = "class:EntityWithNamespaces")
    public static class EntityWithNamespaces {
        @Id
        private URI uri;
    }

    @Test
    void buildsMetamodelOfEntityWithNamespaceUsedOnAttribute() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(EntityWithNamespace.class));
        builder.buildMetamodel(finderMock);
        final EntityType<EntityWithNamespaceAttributes> result =
                (EntityType<EntityWithNamespaceAttributes>) builder.getEntityClass(EntityWithNamespaceAttributes.class);
        assertEquals("http://www.example2.org/EntityWithNamespaceAttributes", result.getIRI().toString());
        final Attribute<? super EntityWithNamespaceAttributes, ?> labelAtt = result.getAttribute("label");
        assertEquals(RDFS.LABEL, labelAtt.getIRI().toString());
        final Attribute<? super EntityWithNamespaceAttributes, ?> descriptionAtt = result.getAttribute("description");
        assertEquals(DC.Elements.DESCRIPTION, descriptionAtt.getIRI().toString());
    }

    @TestLocal
    @Namespaces({@Namespace(prefix = "dc", namespace = DC.Elements.NAMESPACE),
            @Namespace(prefix = "ex2", namespace = "http://www.example2.org/")})
    @OWLClass(iri = "ex2:EntityWithNamespaceAttributes")
    public static class EntityWithNamespaceAttributes {
        @Id
        private URI uri;

        @OWLAnnotationProperty(iri = "rdfs:label")
        private String label;

        @OWLDataProperty(iri = "dc:description")
        private String description;
    }

    @Test
    void buildsMetamodelOfEntityWhichUsesPackageLevelNamespace() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(EntityWithNamespaceFromPackage.class));
        builder.buildMetamodel(finderMock);
        final EntityType<EntityWithNamespaceFromPackage> result = (EntityType<EntityWithNamespaceFromPackage>) builder
                .getEntityClass(EntityWithNamespaceFromPackage.class);
        assertEquals("http://www.example.org/EntityWithNamespaceFromPackage", result.getIRI().toString());
    }

    @TestLocal
    @OWLClass(iri = "ex:EntityWithNamespaceFromPackage")
    public static class EntityWithNamespaceFromPackage {
        @Id
        private URI uri;
    }

    @Test
    void buildMetamodelBuildsResultSetMappersFromMappingConfigurations() {
        when(finderMock.getResultSetMappings())
                .thenReturn(Collections.singleton(OWLClassA.class.getDeclaredAnnotation(SparqlResultSetMapping.class)));
        builder.buildMetamodel(finderMock);
        final ResultSetMappingManager manager = builder.getResultSetMappingManager();
        assertNotNull(manager);
        assertNotNull(manager.getMapper(OWLClassA.VARIABLE_MAPPING));
        verify(finderMock).getResultSetMappings();
    }

    @Test
    void buildMetamodelBuildsEntityWithBuiltInConverters() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassM.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<OWLClassM> et = (IdentifiableEntityType<OWLClassM>) builder.getEntityClass(OWLClassM.class);
        final AbstractPluralAttribute<OWLClassM, Set<Integer>, Integer> result =
                (AbstractPluralAttribute<OWLClassM, Set<Integer>, Integer>) et
                        .getDeclaredAttribute(OWLClassM.getIntegerSetField().getName());
        assertThat(result.getConverter(), instanceOf(ToIntegerConverter.class));
    }

    @Test
    void buildMetamodelBuildsEntityWithLocalDateTimeConverter() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassT.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<OWLClassT> et = (IdentifiableEntityType<OWLClassT>) builder.getEntityClass(OWLClassT.class);
        final AbstractAttribute<OWLClassT, LocalDateTime> result = (AbstractAttribute<OWLClassT, LocalDateTime>) et
                .getDeclaredAttribute(OWLClassT.getLocalDateTimeField().getName());
        assertThat(result.getConverter(), instanceOf(LocalDateTimeConverter.class));
    }

    @Test
    void buildMetamodelBuildsEntityWithObjectConverterForDynamicAttributes() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassV.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<OWLClassV> et = (IdentifiableEntityType<OWLClassV>) builder.getEntityClass(OWLClassV.class);
        final AbstractAttribute<OWLClassV, LocalDateTime> result = (AbstractAttribute<OWLClassV, LocalDateTime>) et
                .getDeclaredAttribute(OWLClassV.getSingularDynamicAttField().getName());
        assertThat(result.getConverter(), instanceOf(ObjectConverter.class));
    }

    @Test
    void buildMetamodelBuildsEntityWithParentLifecycleCallbacks() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(ChildWithCallback.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<ChildWithCallback> result = (IdentifiableEntityType<ChildWithCallback>) builder
                .getEntityClass(ChildWithCallback.class);
        final EntityLifecycleListenerManager childLifecycleManager = result.getLifecycleListenerManager();
        assertFalse(childLifecycleManager.getLifecycleCallbacks().isEmpty());
        assertTrue(childLifecycleManager.hasEntityLifecycleCallback(LifecycleEvent.PRE_PERSIST));
        assertNotNull(childLifecycleManager.getParents());
        assertTrue(childLifecycleManager.getParents()
                                        .stream()
                                        .anyMatch(parent -> parent.hasEntityLifecycleCallback(LifecycleEvent.PRE_PERSIST)));
    }

    @TestLocal
    @MappedSuperclass
    public static class ParentWithCallback {

        @Id
        URI uri;

        @PrePersist
        void prePersistParent() {
        }
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ChildWithCallback")
    public static class ChildWithCallback extends ParentWithCallback {

        @OWLAnnotationProperty(iri = RDFS.LABEL)
        private String label;

        @PrePersist
        void prePersistChild() {
        }
    }

    @Test
    void buildMetamodelSetsLexicalFormConfigOnAttributeMappingBasedOnAnnotation() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassM.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<OWLClassM> result = (IdentifiableEntityType<OWLClassM>) builder.getEntityClass(OWLClassM.class);
        final AbstractAttribute<OWLClassM, String> att = (AbstractAttribute<OWLClassM, String>) result
                .getAttribute(OWLClassM.getLexicalFormField().getName());
        assertTrue(att.isLexicalForm());
    }

    @Test
    void buildMetamodelSetsSimpleLiteralConfigOnAttributeMappingBasedOnAnnotation() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassM.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<OWLClassM> result = (IdentifiableEntityType<OWLClassM>) builder.getEntityClass(OWLClassM.class);
        final AbstractAttribute<OWLClassM, String> att = (AbstractAttribute<OWLClassM, String>) result
                .getAttribute(OWLClassM.getSimpleLiteralField().getName());
        assertTrue(att.isSimpleLiteral());
    }

    @Test
    void buildMetamodelSupportsPluralSimpleLiteralAttributes() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(WithPluralSimpleLiteral.class));
        builder.buildMetamodel(finderMock);
        final IdentifiableEntityType<WithPluralSimpleLiteral> result = (IdentifiableEntityType<WithPluralSimpleLiteral>) builder
                .getEntityClass(WithPluralSimpleLiteral.class);
        final AbstractPluralAttribute<WithPluralSimpleLiteral, Set<String>, String> att = (AbstractPluralAttribute<WithPluralSimpleLiteral, Set<String>, String>) result
                .getAttribute("pluralSimpleLiteral");
        assertNotNull(att);
        assertTrue(att.isSimpleLiteral());
        assertTrue(att.isCollection());
        assertEquals(String.class, att.getElementType().getJavaType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithPluralSimpleLiteral")
    public static class WithPluralSimpleLiteral {

        @Id
        private URI uri;

        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "pluralSimpleLiteral", simpleLiteral = true)
        private Set<String> pluralSimpleLiteral;
    }

    @Test
    void buildMetamodelSupportsCollectionAttributes() {
        when(finderMock.getEntities())
                .thenReturn(new HashSet<>(Arrays.asList(WithCollectionAttributes.class, OWLClassA.class)));
        builder.buildMetamodel(finderMock);

        final IdentifiableEntityType<WithCollectionAttributes> et = (IdentifiableEntityType<WithCollectionAttributes>) builder
                .getEntityClass(WithCollectionAttributes.class);
        assertNotNull(et);
        final PluralAttribute<WithCollectionAttributes, Collection<String>, String> dataAtt = (PluralAttribute<WithCollectionAttributes, Collection<String>, String>) et
                .getAttribute("collectionDataProperty");
        assertNotNull(dataAtt);
        assertThat(dataAtt, instanceOf(CollectionAttribute.class));
        assertEquals(Vocabulary.ATTRIBUTE_BASE + "collectionDataProperty", dataAtt.getIRI().toString());
        assertEquals(CollectionType.COLLECTION, dataAtt.getCollectionType());
        final PluralAttribute<WithCollectionAttributes, Collection<OWLClassA>, OWLClassA> objectAtt = (PluralAttribute<WithCollectionAttributes, Collection<OWLClassA>, OWLClassA>) et
                .getAttribute("collectionObjectProperty");
        assertNotNull(objectAtt);
        assertThat(objectAtt, instanceOf(CollectionAttribute.class));
        assertEquals(Vocabulary.ATTRIBUTE_BASE + "collectionObjectProperty", objectAtt.getIRI().toString());
        assertEquals(CollectionType.COLLECTION, objectAtt.getCollectionType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithCollectionAttributes")
    public static class WithCollectionAttributes {
        @Id
        private URI uri;

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "collectionDataProperty")
        private Collection<String> collectionDataProperty;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "collectionObjectProperty")
        private Collection<OWLClassA> collectionObjectProperty;
    }

    @Test
    void buildMetamodelRegistersReferencesBetweenClassesOnSingularAttributes() {
        when(finderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(OWLClassA.class, OWLClassD.class)));
        builder.buildMetamodel(finderMock);

        assertThat(builder.getTypeReferenceMap().getReferringTypes(OWLClassA.class), hasItem(OWLClassD.class));
        assertTrue(builder.getTypeReferenceMap().getReferringTypes(OWLClassD.class).isEmpty());
    }

    @Test
    void buildMetamodelRegistersReferencesBetweenClassesOnPluralAttributes() {
        when(finderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(OWLClassA.class, OWLClassC.class)));
        builder.buildMetamodel(finderMock);

        assertThat(builder.getTypeReferenceMap().getReferringTypes(OWLClassA.class), hasItem(OWLClassC.class));
        assertTrue(builder.getTypeReferenceMap().getReferringTypes(OWLClassD.class).isEmpty());
    }

    @Test
    void buildMetamodelThrowsInvalidFieldMappingExceptionWhenFieldIsMappedToRDFType() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(WithInvalidTypeField.class));
        assertThrows(InvalidFieldMappingException.class, () -> builder.buildMetamodel(finderMock));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithInvalidTypeField")
    public static class WithInvalidTypeField {
        @Id
        private URI uri;

        @OWLDataProperty(iri = RDF.TYPE)
        private URI type;
    }

    @Test
    void buildMetamodelSupportsMultipleInheritanceInterfaceOnly() {
        when(finderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(AParentI.class, BParentI.class, InterfaceChild.class)));
        builder.buildMetamodel(finderMock);

        final EntityType<InterfaceChild> cClassEt = (EntityType<InterfaceChild>) builder.getEntityClass(InterfaceChild.class);
        final EntityType<AParentI> AParentEt = (EntityType<AParentI>) builder.getEntityClass(AParentI.class);
        final EntityType<BParentI> BParentEt = (EntityType<BParentI>) builder.getEntityClass(BParentI.class);

        assertThat(builder.getEntities().entrySet(), hasSize(3));

        assertNotNull(cClassEt);
        assertThat(cClassEt.getSupertypes(), hasSize(2));
        assertThat(cClassEt.getSupertypes(), containsInAnyOrder(AParentEt, BParentEt));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "AParentI")
    private interface AParentI {
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "BParentI")
    private interface BParentI {
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "InterfaceChild")
    public static class InterfaceChild implements AParentI, BParentI {
        @Id
        private URI uri;
    }

    @Test
    void buildMetamodelSupportsMultipleInheritanceInterfaceAndClass() {
        when(finderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(AParentI.class, BParent.class, ClassChild.class)));
        builder.buildMetamodel(finderMock);

        final EntityType<ClassChild> cClassEt = (EntityType<ClassChild>) builder.getEntityClass(ClassChild.class);
        final EntityType<AParentI> AParentEt = (EntityType<AParentI>) builder.getEntityClass(AParentI.class);
        final EntityType<BParent> BParentEt = (EntityType<BParent>) builder.getEntityClass(BParent.class);

        assertThat(builder.getEntities().entrySet(), hasSize(3));

        assertNotNull(cClassEt);
        assertThat(cClassEt.getSupertypes(), hasSize(2));
        assertThat(cClassEt.getSupertypes(), containsInAnyOrder(AParentEt, BParentEt));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "BParent")
    public static class BParent {
        @Id
        private URI uri;
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "Child")
    public static class ClassChild extends BParent implements AParentI {

    }

    @Test
    void buildMetamodelThrowsExceptionIfAnnotatedMethodHasInvalidName() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(InterfaceWithInvalidMethod.class));

        assertThrows(MetamodelInitializationException.class, () -> builder.buildMetamodel(finderMock));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithInvalidMethod")
    private interface InterfaceWithInvalidMethod {
        @OWLDataProperty(iri = Vocabulary.CLASS_BASE + "name")
        void getName();
    }

    @Test
    void buildMetamodelBuildsAttributeWithConverterForEnumValuedObjectProperty() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassM.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<OWLClassM> et = builder.entity(OWLClassM.class);
        final AbstractAttribute<? super OWLClassM, ?> att = et.getAttribute(OWLClassM.getObjectOneOfEnumAttributeField()
                                                                                     .getName());
        assertNotNull(att.getConverter());
        assertInstanceOf(ObjectOneOfEnumConverter.class, att.getConverter());
        assertEquals(Attribute.PersistentAttributeType.OBJECT, att.getPersistentAttributeType());
    }

    @Test
    void buildMetamodelOverridesFetchTypeOfInferredTypesSpecificationToEager() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(WithInferredTypesAndProperties.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<WithInferredTypesAndProperties> et = builder.entity(WithInferredTypesAndProperties.class);
        final TypesSpecification<? super WithInferredTypesAndProperties, ?> types = et.getTypes();
        assertTrue(types.isInferred());
        assertEquals(FetchType.EAGER, types.getFetchType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithInferredTypesAndProperties")
    public static class WithInferredTypesAndProperties {

        @Id
        private URI uri;

        @Inferred
        @Types(fetchType = FetchType.LAZY)
        private Set<String> types;

        @Inferred
        @Properties(fetchType = FetchType.LAZY)
        private Map<String, Set<String>> properties;
    }

    @Test
    void buildMetamodelOverridesFetchTypeOfInferredPropertiesSpecificationToEager() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(WithInferredTypesAndProperties.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<WithInferredTypesAndProperties> et = builder.entity(WithInferredTypesAndProperties.class);
        final PropertiesSpecification<? super WithInferredTypesAndProperties, ?, ?, ?> types = et.getProperties();
        assertTrue(types.isInferred());
        assertEquals(FetchType.EAGER, types.getFetchType());
    }

    @Test
    void buildMetamodelSupportsClassWithDataPropertyReferencedListAttribute() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(ClassWithDataPropertyReferencedList.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<ClassWithDataPropertyReferencedList> et = builder.entity(ClassWithDataPropertyReferencedList.class);
        final ListAttribute<? super ClassWithDataPropertyReferencedList, MultilingualString> att = et.getList("altLabels", MultilingualString.class);
        assertEquals(SequenceType.referenced, att.getSequenceType());
        assertEquals(RDF.REST, att.getHasNextPropertyIRI().toString());
        assertEquals(RDF.FIRST, att.getHasContentsPropertyIRI().toString());

    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithDataPropertyReferencedList")
    public static class ClassWithDataPropertyReferencedList {
        @Id
        private URI uri;

        @Sequence(type = SequenceType.referenced, hasNextPropertyIRI = RDF.REST,
                  hasContentsPropertyIRI = RDF.FIRST)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "dp-referenced-list")
        private List<MultilingualString> altLabels;
    }

    @Test
    void buildMetamodelThrowsInvalidFieldMappingExceptionWhenAttemptingToUseDataPropertyWithSimpleList() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(ClassWithDataPropertySimpleList.class));
        final InvalidFieldMappingException ex = assertThrows(InvalidFieldMappingException.class, () -> builder.buildMetamodel(finderMock));
        assertThat(ex.getMessage(), containsStringIgnoringCase("simple list"));
        assertThat(ex.getMessage(), containsStringIgnoringCase("must be mapped to an object property"));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithDataPropertySimpleList")
    public static class ClassWithDataPropertySimpleList {
        @Id
        private URI uri;

        @Sequence(type = SequenceType.simple)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "dp-referenced-list")
        private List<MultilingualString> altLabels;
    }

    @Test
    void buildMetamodelSupportsRDFCollectionAttributes() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(ClassWithRDFCollectionAttribute.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<ClassWithRDFCollectionAttribute> et = builder.entity(ClassWithRDFCollectionAttribute.class);
        final ListAttribute<? super ClassWithRDFCollectionAttribute, Integer> result = et.getList("rdfCollection", Integer.class);
        assertInstanceOf(RDFCollectionAttribute.class, result);
        assertEquals(SequenceType.referenced, result.getSequenceType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithRDFCollection")
    public static class ClassWithRDFCollectionAttribute {
        @Id
        private URI uri;

        @RDFCollection
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "rdf-collection")
        private List<Integer> rdfCollection;
    }

    @Test
    void buildMetamodelSupportsRdfContainerAttributes() {
        when(finderMock.getEntities()).thenReturn(Set.of(ClassWithRdfSeqAttribute.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<ClassWithRdfSeqAttribute> et = builder.entity(ClassWithRdfSeqAttribute.class);
        final PluralAttribute<? super ClassWithRdfSeqAttribute, List<Integer>, Integer> result = (PluralAttribute<? super ClassWithRdfSeqAttribute, List<Integer>, Integer>) et.getAttribute("rdfSeq");
        assertInstanceOf(RDFContainerAttribute.class, result);
        final RDFContainerAttribute<? super ClassWithRdfSeqAttribute, List<Integer>, Integer> containerAtt = (RDFContainerAttribute<? super ClassWithRdfSeqAttribute, List<Integer>, Integer>) result;
        assertEquals(CollectionType.LIST, containerAtt.getCollectionType());
        assertEquals(RDFContainerType.SEQ, containerAtt.getContainerType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithRdfSeqAttribute")
    public static class ClassWithRdfSeqAttribute {
        @Id
        private URI uri;

        @RDFContainer(type = RDFContainerType.SEQ)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "rdf-seq")
        private List<Integer> rdfSeq;
    }

    @Test
    void buildMetamodelThrowsInvalidFieldMappingExceptionWhenAttemptingToUseRDFCollectionWithInference() {
        when(finderMock.getEntities()).thenReturn(Set.of(ClassWithInferredRdfContainerAttribute.class));
        final InvalidFieldMappingException ex = assertThrows(InvalidFieldMappingException.class, () -> builder.buildMetamodel(finderMock));
        assertThat(ex.getMessage(), containsString("RDF container"));
        assertThat(ex.getMessage(), containsString("inferred"));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithRdfSeqAttribute")
    public static class ClassWithInferredRdfContainerAttribute {
        @Id
        private URI uri;

        @Inferred
        @RDFContainer(type = RDFContainerType.ALT)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "rdf-alt")
        private Set<Integer> rdfAlt;
    }

    @Test
    void buildMetamodelSupportsGenericsInAbstractSuperclass() {
        when(finderMock.getEntities()).thenReturn(Set.of(OWLClassA.class, OWLClassB.class, ClassWithGenericType.class, ConcreteClassWithGenericType.class, ConcreteClassWithGenericTypeII.class));
        builder.buildMetamodel(finderMock);
        final EntityType<ConcreteClassWithGenericType> et = (EntityType<ConcreteClassWithGenericType>) builder.getEntityClass(ConcreteClassWithGenericType.class);
        final Attribute<ConcreteClassWithGenericType, ?> att = (Attribute<ConcreteClassWithGenericType, ?>) et.getAttribute("boss");
        final SetAttribute<ConcreteClassWithGenericType, ?> setAtt = (SetAttribute<ConcreteClassWithGenericType, ?>) et.getAttribute("values");
        assertEquals(OWLClassA.class, att.getJavaType());
        assertEquals(builder.getEntityClass(OWLClassA.class), setAtt.getElementType());
        final EntityType<ConcreteClassWithGenericTypeII> etII = (EntityType<ConcreteClassWithGenericTypeII>) builder.getEntityClass(ConcreteClassWithGenericTypeII.class);
        final Attribute<ConcreteClassWithGenericTypeII, ?> attII = (Attribute<ConcreteClassWithGenericTypeII, ?>) etII.getAttribute("boss");
        final SetAttribute<ConcreteClassWithGenericTypeII, ?> setAttII = (SetAttribute<ConcreteClassWithGenericTypeII, ?>) etII.getAttribute("values");
        assertEquals(OWLClassB.class, attII.getJavaType());
        assertEquals(builder.getEntityClass(OWLClassB.class), setAttII.getElementType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithGenericType")
    public static abstract class ClassWithGenericType<T extends HasUri> {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "generic-value")
        private T boss;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "generic-values")
        private Set<T> values;
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ConcreteClassWithGenericType")
    public static class ConcreteClassWithGenericType extends ClassWithGenericType<OWLClassA> {
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ConcreteClassWithGenericTypeII")
    public static class ConcreteClassWithGenericTypeII extends ClassWithGenericType<OWLClassB> {
    }

    @Test
    void buildMetamodelSupportsGenericsInAbstractSuperSuperclass() {
        when(finderMock.getEntities()).thenReturn(Set.of(OWLClassA.class, OWLClassB.class, OWLClassR.class, OWLClassS.class, ClassWithGenericType.class, ConcreteClassWithGenericType.class, ConcreteClassWithGenericTypeII.class, SubClassWithGenericType.class, ConcreteClassWithGenericTypeIII.class));
        builder.buildMetamodel(finderMock);
        final EntityType<ConcreteClassWithGenericTypeIII> et = (EntityType<ConcreteClassWithGenericTypeIII>) builder.getEntityClass(ConcreteClassWithGenericTypeIII.class);
        final Attribute<? super ConcreteClassWithGenericTypeIII, ?> att = et.getAttribute("boss");
        final SetAttribute<? super ConcreteClassWithGenericTypeIII, ?> setAtt = et.getSet("values");
        assertEquals(OWLClassR.class, att.getJavaType());
        assertEquals(OWLClassR.class, setAtt.getBindableJavaType());
    }


    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "SubClassWithGenericType")
    public static abstract class SubClassWithGenericType<T extends OWLClassS> extends ClassWithGenericType<T> {
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ConcreteClassWithGenericTypeIII")
    public static class ConcreteClassWithGenericTypeIII extends SubClassWithGenericType<OWLClassR> {
    }

    @Test
    void buildMetamodelSupportsMultipleGenericParametersInAbstractSuperclass() {
        when(finderMock.getEntities()).thenReturn(Set.of(OWLClassA.class, OWLClassB.class, ClassWithTwoGenericTypes.class, ConcreteClassWithTwoGenericTypes.class));
        builder.buildMetamodel(finderMock);
        final EntityType<ConcreteClassWithTwoGenericTypes> et = (EntityType<ConcreteClassWithTwoGenericTypes>) builder.getEntityClass(ConcreteClassWithTwoGenericTypes.class);
        final SetAttribute<? super ConcreteClassWithTwoGenericTypes, ?> tAtt = et.getSet("tValues");
        assertEquals(OWLClassA.class, tAtt.getBindableJavaType());
        final SetAttribute<? super ConcreteClassWithTwoGenericTypes, ?> vAtt = et.getSet("vValues");
        assertEquals(OWLClassB.class, vAtt.getBindableJavaType());
    }

    @TestLocal
    @MappedSuperclass
    public static abstract class ClassWithTwoGenericTypes<T extends HasUri, V extends HasUri> {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "fieldOfTypeT")
        private Set<T> tValues;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "fieldOfTypeV")
        private Set<V> vValues;
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ConcreteClassWithTwoGenericTypes")
    public static class ConcreteClassWithTwoGenericTypes extends ClassWithTwoGenericTypes<OWLClassA, OWLClassB> {
    }

    @Test
    void buildMetamodelSupportsQueryAttributesWithGenericType() {
        when(finderMock.getEntities()).thenReturn(Set.of(OWLClassA.class, OWLClassB.class, ClassWithGenericTypeAndQueryAttribute.class, ConcreteClassWithQueryAttribute.class, ConcreteClassWithQueryAttributeII.class));
        builder.buildMetamodel(finderMock);

        final EntityType<ConcreteClassWithQueryAttribute> et = (EntityType<ConcreteClassWithQueryAttribute>) builder.getEntityClass(ConcreteClassWithQueryAttribute.class);
        final PluralQueryAttribute<? super ConcreteClassWithQueryAttribute, ?, ?> att = (PluralQueryAttribute<? super ConcreteClassWithQueryAttribute, ?, ?>) et.getQueryAttribute("related");
        assertEquals(OWLClassA.class, att.getBindableJavaType());
        final EntityType<ConcreteClassWithQueryAttributeII> etII = (EntityType<ConcreteClassWithQueryAttributeII>) builder.getEntityClass(ConcreteClassWithQueryAttributeII.class);
        final PluralQueryAttribute<? super ConcreteClassWithQueryAttributeII, ?, ?> attII = (PluralQueryAttribute<? super ConcreteClassWithQueryAttributeII, ?, ?>) etII.getQueryAttribute("related");
        assertEquals(OWLClassB.class, attII.getBindableJavaType());
    }

    @TestLocal
    @MappedSuperclass
    public static abstract class ClassWithGenericTypeAndQueryAttribute<T extends HasUri> {
        @Id
        private URI id;

        @Sparql(query = "SELECT ?x WHERE { ?x skos:related ?this }")
        private Set<T> related;
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ConcreteClassWithQueryAttribute")
    public static class ConcreteClassWithQueryAttribute extends ClassWithGenericTypeAndQueryAttribute<OWLClassA> {
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ConcreteClassWithQueryAttributeII")
    public static class ConcreteClassWithQueryAttributeII extends ClassWithGenericTypeAndQueryAttribute<OWLClassB> {
    }

    @Test
    void buildMetamodelFinishesBuildingEntityTypes() {
        when(finderMock.getEntities()).thenReturn(Set.of(OWLClassA.class));
        builder.buildMetamodel(finderMock);
        final AbstractIdentifiableType<OWLClassA> a = (AbstractIdentifiableType<OWLClassA>) builder.getEntityClass(OWLClassA.class);
        assertNotNull(a.getSubtypes());
        assertTrue(a.getSubtypes().isEmpty());
    }
}
