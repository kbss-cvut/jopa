package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.Subgraph;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FetchGraphProcessorTest {

    @Mock
    private MetamodelImpl metamodel;

    private static MetamodelMocks metamodelMocks;

    @BeforeAll
    static void setUpBeforeAll() throws Exception {
        metamodelMocks = new MetamodelMocks();
    }

    @BeforeEach
    void setUp() {
        metamodelMocks.setMocks(metamodel);
    }

    @Test
    void mapFetchGraphToProjectionCreatesMappingsForAttributesOfSingleEntityClass() {
        final FetchGraphProcessor sut = new FetchGraphProcessor(metamodel);
        final EntityGraph<OWLClassA> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassA()
                                                                                      .entityType(), metamodel);
        fetchGraph.addAttributeNodes(metamodelMocks.forOwlClassA().stringAttribute());
        final List<FetchGraphProcessor.QueryProjectionToAxiomMapping> result = sut.mapFetchGraphToProjection(fetchGraph, metamodelMocks.forOwlClassA()
                                                                                                                                       .entityType(), "x");
        assertThat(result, hasItems(new FetchGraphProcessor.QueryProjectionToAxiomMapping("x", "x_stringAttribute", metamodelMocks.forOwlClassA()
                                                                                                                                  .stringAttribute()),
                new FetchGraphProcessor.QueryProjectionToAxiomMapping("x", "x_types", metamodelMocks.forOwlClassA()
                                                                                                    .typesSpec())));
    }

    @Test
    void mapFetchGraphToProjectionCreatesMappingsForAttributesOfTwoLinkedEntities() {
        final FetchGraphProcessor sut = new FetchGraphProcessor(metamodel);
        final EntityGraph<OWLClassD> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassD()
                                                                                      .entityType(), metamodel);
        final Subgraph<OWLClassA> sg = fetchGraph.addSubgraph(metamodelMocks.forOwlClassD().owlClassAAtt());
        sg.addAttributeNodes(metamodelMocks.forOwlClassA().stringAttribute());
        final List<FetchGraphProcessor.QueryProjectionToAxiomMapping> result = sut.mapFetchGraphToProjection(fetchGraph, metamodelMocks.forOwlClassD()
                                                                                                                                       .entityType(), "x");
        assertThat(result, hasItems(
                new FetchGraphProcessor.QueryProjectionToAxiomMapping("x", "x_owlClassA", metamodelMocks.forOwlClassD()
                                                                                                        .owlClassAAtt()),
                new FetchGraphProcessor.QueryProjectionToAxiomMapping("x_owlClassA", "x_owlClassA_stringAttribute", metamodelMocks.forOwlClassA()
                                                                                                                                  .stringAttribute()),
                new FetchGraphProcessor.QueryProjectionToAxiomMapping("x_owlClassA", "x_owlClassA_types", metamodelMocks.forOwlClassA()
                                                                                                                        .typesSpec())));
    }
}
