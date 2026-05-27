package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.NamedAttributeNode;
import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph;
import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraphs;
import cz.cvut.kbss.jopa.model.annotations.NamedSubgraph;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.MetamodelClassMapper;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class NamedEntityGraphProcessorTest {

    @Mock
    private MetamodelClassMapper metamodelMapper;

    @InjectMocks
    private NamedEntityGraphProcessor processor;

    @Test
    void buildEntityGraphBuildsEntityGraphFromAnnotationDeclaringAttributeNodes() {
        final AbstractIdentifiableType<EntityWithAttributes> ait = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(EntityWithAttributes.class)).thenReturn(ait);
        final AbstractAttribute nameAtt = mock(AbstractAttribute.class);
        when(nameAtt.getName()).thenReturn("name");
        final AbstractAttribute descriptionAtt = mock(AbstractAttribute.class);
        when(descriptionAtt.getName()).thenReturn("description");
        when(ait.getAttribute("name")).thenReturn(nameAtt);
        when(ait.getAttribute("description")).thenReturn(descriptionAtt);
        final NamedEntityGraphs ne = EntityWithAttributes.class.getAnnotation(NamedEntityGraphs.class);
        processor.buildEntityGraph(EntityWithAttributes.class, ne.value()[1]);

        final EntityGraph<EntityWithAttributes> eg = (EntityGraph<EntityWithAttributes>) processor.getManager()
                                                                                                  .getEntityGraph("ExplicitlyNamedEntityWithAttributes");
        assertNotNull(eg);
        assertEquals(2, eg.getAttributeNodes().size());
        final List<String> attributeNames = eg.getAttributeNodes().stream().map(AttributeNode::getAttributeName)
                                              .collect(Collectors.toList());
        assertThat(attributeNames, containsInAnyOrder("name", "description"));
    }

    @NamedEntityGraph(attributeNodes = {
            @NamedAttributeNode("name")
    })
    @NamedEntityGraph(name = "ExplicitlyNamedEntityWithAttributes", attributeNodes = {
            @NamedAttributeNode("name"),
            @NamedAttributeNode("description")
    })
    @TestLocal
    public static class EntityWithAttributes {
        @Id
        private URI uri;

        @OWLAnnotationProperty(iri = RDFS.LABEL)
        private String name;

        @OWLAnnotationProperty(iri = RDFS.COMMENT)
        private String description;
    }

    @Test
    void buildEntityGraphDefaultNameToAnnotatedEntityClassName() {
        final AbstractIdentifiableType<EntityWithAttributes> ait = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(EntityWithAttributes.class)).thenReturn(ait);
        when(ait.getAttribute(anyString())).thenAnswer(inv -> mock(AbstractAttribute.class));
        final NamedEntityGraphs ne = EntityWithAttributes.class.getAnnotation(NamedEntityGraphs.class);
        processor.buildEntityGraph(EntityWithAttributes.class, ne.value()[0]);

        final EntityGraph<EntityWithAttributes> result = (EntityGraph<EntityWithAttributes>) processor.getManager()
                                                                                                  .getEntityGraph(EntityWithAttributes.class.getSimpleName());
        assertNotNull(result);
    }

    @Test
    void buildEntityGraphSupportsReferenceToOtherEntityGraph() {
        final AbstractIdentifiableType<ReferencingEntity> rootAit = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(ReferencingEntity.class)).thenReturn(rootAit);
        final AbstractAttribute referencedEntityAtt = mock(AbstractAttribute.class);
        when(referencedEntityAtt.getJavaType()).thenReturn(EntityWithAttributes.class);
        when(rootAit.getAttribute("referencedEntity")).thenReturn(referencedEntityAtt);
        final AbstractIdentifiableType<EntityWithAttributes> subgraphAit = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(EntityWithAttributes.class)).thenReturn(subgraphAit);
        when(subgraphAit.getAttribute("name")).thenReturn(mock(AbstractAttribute.class));
        when(subgraphAit.getJavaType()).thenReturn(EntityWithAttributes.class);
        final NamedEntityGraph ne = ReferencingEntity.class.getAnnotation(NamedEntityGraph.class);
        processor.buildEntityGraph(ReferencingEntity.class, ne);

        final EntityGraph<ReferencingEntity> result = (EntityGraph<ReferencingEntity>) processor.getManager()
                                                                                            .getEntityGraph("ReferencingEntity");
        assertNotNull(result);
        assertEquals(1, result.getAttributeNodes().size());
        final Subgraph<EntityWithAttributes> subgraph = result.getAttributeNodes().get(0).getSubgraphs()
                                                          .get(EntityWithAttributes.class);
        assertNotNull(subgraph);
        assertEquals(1, subgraph.getAttributeNodes().size());
    }


    @NamedEntityGraph(
            attributeNodes = {@NamedAttributeNode(value = "referencedEntity", subgraph = "referencedEntity")},
            subgraphs = {
                    @NamedSubgraph(name = "referencedEntity",
                                   attributeNodes = {@NamedAttributeNode("name")})
            })
    @TestLocal
    public static class ReferencingEntity {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "referencedEntity")
        private EntityWithAttributes referencedEntity;
    }

    @Test
    void buildEntityGraphSupportsTraversalAcrossMultipleLevels() {
        final AbstractIdentifiableType<ReferencingReferencingEntity> rootAit = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(ReferencingReferencingEntity.class)).thenReturn(rootAit);
        final AbstractAttribute referencedEntityAtt = mock(AbstractAttribute.class);
        when(rootAit.getAttribute("referencedEntity")).thenReturn(referencedEntityAtt);
        final AbstractIdentifiableType<ReferencingEntity> middleAit = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(ReferencingEntity.class)).thenReturn(middleAit);
        when(middleAit.getJavaType()).thenReturn(ReferencingEntity.class);
        final AbstractAttribute middleReferencedEntityAtt = mock(AbstractAttribute.class);
        when(middleAit.getAttribute("referencedEntity")).thenReturn(middleReferencedEntityAtt);
        final AbstractIdentifiableType<EntityWithAttributes> subgraphAit = mock(AbstractIdentifiableType.class);
        when(metamodelMapper.entity(EntityWithAttributes.class)).thenReturn(subgraphAit);
        final AbstractAttribute nameAtt = mock(AbstractAttribute.class);
        when(nameAtt.getName()).thenReturn("name");
        when(subgraphAit.getAttribute("name")).thenReturn(nameAtt);
        when(subgraphAit.getJavaType()).thenReturn(EntityWithAttributes.class);
        final NamedEntityGraph ne = ReferencingReferencingEntity.class.getAnnotation(NamedEntityGraph.class);
        processor.buildEntityGraph(ReferencingReferencingEntity.class, ne);

        final EntityGraph<ReferencingReferencingEntity> result = (EntityGraph<ReferencingReferencingEntity>) processor.getManager()
                                                                                                                  .getEntityGraph("RootReferencingEntity");
        assertNotNull(result);
        assertEquals(1, result.getAttributeNodes().size());
        final Subgraph<ReferencingEntity> subgraph = result.getAttributeNodes().get(0).getSubgraphs()
                                                       .get(ReferencingEntity.class);
        assertNotNull(subgraph);
        assertEquals(1, subgraph.getAttributeNodes().size());
        final Subgraph<EntityWithAttributes> subSubgraph = subgraph.getAttributeNodes().get(0).getSubgraphs()
                                                                   .get(EntityWithAttributes.class);
        assertNotNull(subSubgraph);
        assertEquals(1, subSubgraph.getAttributeNodes().size());
        assertEquals("name", subSubgraph.getAttributeNodes().get(0).getAttributeName());
    }

    @NamedEntityGraph(name = "RootReferencingEntity", attributeNodes = {
            @NamedAttributeNode(value = "referencedEntity", subgraph = "root.referencedEntity")
    }, subgraphs = {
            @NamedSubgraph(name = "root.referencedEntity",
                           attributeNodes = {@NamedAttributeNode(value = "referencedEntity",
                                                                 subgraph = "root.referencedEntity.referencedEntity")},
                           type = ReferencingEntity.class),
            @NamedSubgraph(name = "root.referencedEntity.referencedEntity",
                           attributeNodes = {@NamedAttributeNode("name")}, type = EntityWithAttributes.class)
    })
    @TestLocal
    public static class ReferencingReferencingEntity {
        @Id
        private URI uri;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "referencedEntity")
        private ReferencingEntity referencedEntity;
    }
}
