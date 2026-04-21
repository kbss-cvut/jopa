package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;
import java.util.Optional;

import static cz.cvut.kbss.jopa.utils.IdentifierTransformer.stringifyIri;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class EntityMappingQueryModifierTest {

    private MetamodelMocks metamodelMocks;

    @Mock
    private MetamodelImpl metamodel;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
    }

    @Test
    void modifyGeneratesBasicGraphPatternForSpecifiedFetchGraph() {
        final EntityMappingQueryModifier sut = new EntityMappingQueryModifier(metamodel, metamodelMocks.forOwlClassD()
                                                                                                       .entityType(),
                new EntityDescriptor(), false);
        final EntityGraph<OWLClassD> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassD()
                                                                                      .entityType(), metamodel);
        fetchGraph.addAttributeNodes("owlClassA");
        final EntityMappingQueryModifier.QueryModification result = sut.modify(fetchGraph, "x");
        assertEquals(List.of("x_owlClassA", "x_types"), result.variableNames());
        assertThat(result.queryPart(), containsString("OPTIONAL { ?x " + stringifyIri(Vocabulary.P_HAS_A) + " ?x_owlClassA . }"));
        assertThat(result.queryPart(), containsString("?x a " + stringifyIri(Vocabulary.c_OwlClassD) + " ."));
    }

    @Test
    void modifyGeneratesSubGraphPatternForSpecifiedFetchGraph() {
        final EntityMappingQueryModifier sut = new EntityMappingQueryModifier(metamodel, metamodelMocks.forOwlClassD()
                                                                                                       .entityType(),
                new EntityDescriptor(), false);
        final EntityGraph<OWLClassD> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassD()
                                                                                      .entityType(), metamodel);
        final Subgraph<OWLClassA> sg = fetchGraph.addSubgraph("owlClassA");
        sg.addAttributeNodes("stringAttribute");
        final EntityMappingQueryModifier.QueryModification result = sut.modify(fetchGraph, "x");
        assertThat(result.variableNames(), hasItems("x_owlClassA", "x_owlClassA_stringAttribute", "x_owlClassA_types"));
        assertThat(result.queryPart(), containsString("?x a " + stringifyIri(Vocabulary.c_OwlClassD) + " ."));
        assertThat(result.queryPart(), containsString("OPTIONAL { ?x " + stringifyIri(Vocabulary.P_HAS_A) + " ?x_owlClassA . " +
                "?x_owlClassA a ?x_owlClassA_types . " +
                "OPTIONAL { ?x_owlClassA " + stringifyIri(Vocabulary.p_a_stringAttribute) + " ?x_owlClassA_stringAttribute . } }"));
    }

    @Test
    void modifyGeneratesExactTypeTriplePatternWhenEntityClassDoesNotHaveTypesField() {
        final EntityMappingQueryModifier sut = new EntityMappingQueryModifier(metamodel, metamodelMocks.forOwlClassE()
                                                                                                       .entityType(),
                new EntityDescriptor(), false);
        final EntityGraph<OWLClassE> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassE()
                                                                                      .entityType(), metamodel);
        fetchGraph.addAttributeNodes("stringAttribute");
        final EntityMappingQueryModifier.QueryModification result = sut.modify(fetchGraph, "x");
        assertThat(result.queryPart(), containsString("?x a " + stringifyIri(Vocabulary.c_OwlClassE) + " ."));
    }

    @Test
    void modifyBindsEntityTypeAndGeneratesQueryVariableMappingForItWhenEntityClassHasNoTypesField() {
        final EntityMappingQueryModifier sut = new EntityMappingQueryModifier(metamodel, metamodelMocks.forOwlClassE()
                                                                                                       .entityType(),
                new EntityDescriptor(), false);
        final EntityGraph<OWLClassE> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassE()
                                                                                      .entityType(), metamodel);
        fetchGraph.addAttributeNodes("stringAttribute");
        final EntityMappingQueryModifier.QueryModification result = sut.modify(fetchGraph, "x");
        assertEquals(List.of("x_stringAttribute", "x_types"), result.variableNames());
        final Optional<QueryVariableMapping> typesMapping = result.variables().stream()
                                                                  .filter(QueryVariableMapping::isTypes)
                                                                  .findFirst();
        assertTrue(typesMapping.isPresent());
        assertNull(typesMapping.get().attribute());
        assertThat(result.queryPart(), containsString("BIND (" + stringifyIri(Vocabulary.c_OwlClassE) + " AS ?x_types)"));
    }
}
