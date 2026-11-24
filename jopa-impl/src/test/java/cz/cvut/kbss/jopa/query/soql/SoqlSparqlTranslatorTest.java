package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.query.QueryType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Execution(ExecutionMode.CONCURRENT)
class SoqlSparqlTranslatorTest {

    @Test
    void translatesNestedConditionGroupsWithoutFlatteningDisjunctions() {
        final SoqlCondition first = predicate("?x <urn:first> ?first . ");
        final SoqlCondition second = predicate("?x <urn:second> ?second . ");
        final SoqlCondition third = predicate("?x <urn:third> ?third . ");
        final SoqlCondition condition = new SoqlCondition.Conjunction(List.of(
                first, new SoqlCondition.Disjunction(List.of(second, third))));
        assertThat(translator(condition).translate(), equalToCompressingWhiteSpace(
                "SELECT ?x WHERE { ?x a <urn:Person> . ?x <urn:first> ?first . " +
                        "{ ?x <urn:second> ?second . } UNION { ?x <urn:third> ?third . } }"));
    }

    @Test
    void deduplicatesWithinConjunctionButNotAcrossDisjunctionBranches() {
        final SoqlCondition condition = predicate("?x <urn:value> ?value . ");
        final SoqlCondition repeated = new SoqlCondition.Conjunction(List.of(condition, condition));
        assertThat(translator(new SoqlCondition.Disjunction(List.of(repeated, condition))).translate(),
                equalToCompressingWhiteSpace("SELECT ?x WHERE { ?x a <urn:Person> . " +
                        "{ ?x <urn:value> ?value . } UNION { ?x <urn:value> ?value . } }"));
    }

    @Test
    void bindsNegativeOnlyDisjunctionBranchLocally() {
        final SoqlCondition condition = new SoqlCondition.Disjunction(List.of(
                new SoqlCondition.Negation(predicate("?x <urn:first> ?first . ")),
                predicate("?x <urn:second> ?second . ")));
        assertThat(translator(condition).translate(), equalToCompressingWhiteSpace(
                "SELECT ?x WHERE { ?x a <urn:Person> . " +
                        "{ ?x a <urn:Person> . FILTER NOT EXISTS { ?x <urn:first> ?first . } } UNION " +
                        "{ ?x <urn:second> ?second . } }"));
    }

    @Test
    void translationDoesNotConsumeOrMutateTheQueryModel() {
        final SoqlSparqlTranslator translator = translator(predicate("?x <urn:value> ?value . "));
        assertEquals(translator.translate(), translator.translate());
    }

    @Test
    void conditionGroupsSnapshotTheirChildren() {
        final List<SoqlCondition> children = new ArrayList<>();
        final SoqlCondition.Conjunction conjunction = new SoqlCondition.Conjunction(children);
        children.add(predicate("?x <urn:value> ?value . "));
        assertEquals(List.of(), conjunction.children());
        assertThrows(UnsupportedOperationException.class, () -> conjunction.children().clear());
    }

    private SoqlSparqlTranslator translator(SoqlCondition condition) {
        final SoqlQueryModel query = new SoqlQueryModel(QueryType.SELECT, new SelectProjection(null, false, false),
                "?x", "?x", "p", List.of(attribute("?x a <urn:Person> . ")), condition,
                List.of(), List.of(), List.of(), List.of());
        return new SoqlSparqlTranslator(query);
    }

    private SoqlCondition predicate(String triple) {
        return new SoqlCondition.Predicate(attribute(triple));
    }

    private SoqlAttribute attribute(String triple) {
        final SoqlAttribute attribute = mock(SoqlAttribute.class);
        when(attribute.getBasicGraphPattern(eq("?x"), eq("p"), anyMap())).thenReturn(List.of(triple));
        return attribute;
    }
}
