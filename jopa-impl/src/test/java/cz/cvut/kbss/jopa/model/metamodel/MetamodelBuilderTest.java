/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
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
import java.util.*;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
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

    @Namespace(prefix = "class", namespace = Vocabulary.CLASS_BASE)
    @OWLClass(iri = "class:EntityWithNamespace")
    private static class EntityWithNamespace {
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

    @Namespaces({@Namespace(prefix = "class", namespace = Vocabulary.CLASS_BASE)})
    @OWLClass(iri = "class:EntityWithNamespaces")
    private static class EntityWithNamespaces {
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

    @Namespaces({@Namespace(prefix = "dc", namespace = DC.Elements.NAMESPACE),
            @Namespace(prefix = "ex2", namespace = "http://www.example2.org/")})
    @OWLClass(iri = "ex2:EntityWithNamespaceAttributes")
    private static class EntityWithNamespaceAttributes {
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

    @OWLClass(iri = "ex:EntityWithNamespaceFromPackage")
    private static class EntityWithNamespaceFromPackage {
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
        assertTrue(childLifecycleManager.hasLifecycleCallback(LifecycleEvent.PRE_PERSIST));
        assertNotNull(childLifecycleManager.getParents());
        assertTrue(childLifecycleManager.getParents()
                                        .stream()
                                        .anyMatch(parent -> parent.hasLifecycleCallback(LifecycleEvent.PRE_PERSIST)));
    }

    @MappedSuperclass
    public static class ParentWithCallback {

        @Id
        URI uri;

        @PrePersist
        void prePersistParent() {
        }
    }

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

    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithPluralSimpleLiteral")
    private static class WithPluralSimpleLiteral {

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

    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithCollectionAttributes")
    private static class WithCollectionAttributes {
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

    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithInvalidTypeField")
    private static class WithInvalidTypeField {
        @Id
        private URI uri;

        @OWLDataProperty(iri = RDF.TYPE)
        private URI type;
    }
}
