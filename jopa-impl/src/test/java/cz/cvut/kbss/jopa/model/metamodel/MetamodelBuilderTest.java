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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.annotations.*;
import org.junit.Test;

import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

@SuppressWarnings("unused")
public class MetamodelBuilderTest {

    private final MetamodelBuilder builder = new MetamodelBuilder();

    @Test
    public void buildsMetamodelOfEntityWithSingleNamespaceDeclaredOnClass() {
        builder.buildMetamodel(Collections.singleton(EntityWithNamespace.class));
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
    public void buildsMetamodelOfEntityWithNamespacesDeclaredOnClass() {
        builder.buildMetamodel(Collections.singleton(EntityWithNamespaces.class));
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
    public void buildsMetamodelOfEntityWithNamespaceUsedOnAttribute() {
        builder.buildMetamodel(Collections.singleton(EntityWithNamespaceAttributes.class));
        final EntityType<EntityWithNamespaceAttributes> result =
                (EntityType<EntityWithNamespaceAttributes>) builder.getEntityClass(EntityWithNamespaceAttributes.class);
        assertEquals("http://www.example2.org/EntityWithNamespaceAttributes", result.getIRI().toString());
        final Attribute<? super EntityWithNamespaceAttributes, ?> labelAtt = result.getAttribute("label");
        assertEquals(CommonVocabulary.RDFS_LABEL, labelAtt.getIRI().toString());
        final Attribute<? super EntityWithNamespaceAttributes, ?> descriptionAtt = result.getAttribute("description");
        assertEquals(CommonVocabulary.DC_DESCRIPTION, descriptionAtt.getIRI().toString());
    }

    @Namespaces({@Namespace(prefix = "dc", namespace = "http://purl.org/dc/elements/1.1/"),
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
    public void buildsMetamodelOfEntityWhichUsesPackageLevelNamespace() {
        builder.buildMetamodel(Collections.singleton(EntityWithNamespaceFromPackage.class));
        final EntityType<EntityWithNamespaceFromPackage> result = (EntityType<EntityWithNamespaceFromPackage>) builder
                .getEntityClass(EntityWithNamespaceFromPackage.class);
        assertEquals("http://www.example.org/EntityWithNamespaceFromPackage", result.getIRI().toString());
    }

    @OWLClass(iri = "ex:EntityWithNamespaceFromPackage")
    private static class EntityWithNamespaceFromPackage {
        @Id
        private URI uri;
    }
}