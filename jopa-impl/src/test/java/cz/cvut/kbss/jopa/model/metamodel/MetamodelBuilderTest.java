/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.Vocabulary;
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
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
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
}
