/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import cz.cvut.kbss.jopa.oom.converter.LocalDateConverter;
import cz.cvut.kbss.jopa.oom.converter.ToIntegerConverter;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.time.LocalDate;
import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings("unused")
public class MetamodelBuilderTest {

    @Mock
    private PersistenceUnitClassFinder finderMock;

    private final MetamodelBuilder builder = new MetamodelBuilder();

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
    }

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
        final EntityTypeImpl<OWLClassM> et = (EntityTypeImpl<OWLClassM>) builder.getEntityClass(OWLClassM.class);
        final AbstractPluralAttribute<OWLClassM, Set, Integer> result =
                (AbstractPluralAttribute<OWLClassM, Set, Integer>) et
                        .getDeclaredAttribute(OWLClassM.getIntegerSetField().getName());
        assertTrue(result.getConverter() instanceof ToIntegerConverter);
    }

    @Test
    void buildMetamodelBuildsEntityWithLocalDateConverter() throws Exception {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(OWLClassT.class));
        builder.buildMetamodel(finderMock);
        final EntityTypeImpl<OWLClassT> et = (EntityTypeImpl<OWLClassT>) builder.getEntityClass(OWLClassT.class);
        final AbstractAttribute<OWLClassT, LocalDate> result = (AbstractAttribute<OWLClassT, LocalDate>) et
                .getDeclaredAttribute(OWLClassT.getLocalDateField().getName());
        assertTrue(result.getConverter() instanceof LocalDateConverter);
    }

    @Test
    void buildMetamodelBuildsEntityWithParentLifecycleCallbacks() {
        when(finderMock.getEntities()).thenReturn(Collections.singleton(ChildWithCallback.class));
        builder.buildMetamodel(finderMock);
        final EntityTypeImpl<ChildWithCallback> result = (EntityTypeImpl<ChildWithCallback>) builder
                .getEntityClass(ChildWithCallback.class);
        final EntityLifecycleListenerManager childLifecycleManager = result.getLifecycleListenerManager();
        assertFalse(childLifecycleManager.getLifecycleCallbacks().isEmpty());
        assertTrue(childLifecycleManager.hasLifecycleCallback(LifecycleEvent.PRE_PERSIST));
        assertNotNull(childLifecycleManager.getParent());
        assertTrue(childLifecycleManager.getParent().hasLifecycleCallback(LifecycleEvent.PRE_PERSIST));
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
}