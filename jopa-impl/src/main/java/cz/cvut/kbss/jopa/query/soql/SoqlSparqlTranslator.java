package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.query.QueryType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

final class SoqlSparqlTranslator {

    private final SoqlQueryModel query;
    private final Map<FieldSpecification<?, ?>, TriplePatternEnhancer> triplePatternEnhancers = new HashMap<>();

    SoqlSparqlTranslator(SoqlQueryModel query) {
        this.query = query;
    }

    //Methods to build new Query
    String translate() {
        final StringBuilder result = new StringBuilder(query.type().getKeyword());
        if (query.type() == QueryType.SELECT) {
            appendProjection(result);
        }
        query.defaultGraphs().forEach(iri -> result.append(" FROM ").append(iri));
        query.namedGraphs().forEach(iri -> result.append(" FROM NAMED ").append(iri));
        final Set<String> requiredPatterns = new LinkedHashSet<>();
        query.requiredAttributes().forEach(attribute -> requiredPatterns.addAll(triples(attribute)));
        result.append(" WHERE { ").append(String.join("", requiredPatterns));
        result.append(render(query.condition(), requiredPatterns, false)).append('}');
        if (!query.grouping().isEmpty()) {
            result.append(" GROUP BY");
            query.grouping().forEach(group -> result.append(' ').append(group.getGroupByPart(query.rootVariable())));
        }
        if (!query.ordering().isEmpty()) {
            result.append(" ORDER BY");
            query.ordering().forEach(order -> result.append(' ').append(order.getOrderByPart(query.rootVariable())));
        }
        return result.toString();
    }

    private void appendProjection(StringBuilder result) {
        final SelectProjection projection = query.projection();
        if (projection.distinct()) {
            result.append(" DISTINCT");
        }
        if (projection.aggregateFunction() == null) {
            result.append(' ').append(query.selectedVariable());
            return;
        }
        final AggregateFunction aggregate = projection.aggregateFunction();
        result.append(" (").append(aggregate.soqlName()).append('(');
        if (projection.aggregateDistinct()) {
            result.append("DISTINCT ");
        }
        result.append(query.selectedVariable()).append(") AS ").append(aggregate.resultVariable()).append(')');
    }

    private String render(SoqlCondition condition, Set<String> inheritedPatterns, boolean unionBranch) {
        return switch (condition) {
            case SoqlCondition.Disjunction disjunction -> disjunction.children().stream()
                                                                     .map(child -> "{ " + render(child, inheritedPatterns, true) + "} ")
                                                                     .collect(Collectors.joining("UNION "));
            case SoqlCondition.Conjunction conjunction ->
                    renderConjunction(conjunction.children(), inheritedPatterns, unionBranch);
            case SoqlCondition.Predicate predicate ->
                    renderConjunction(List.of(predicate), inheritedPatterns, unionBranch);
            case SoqlCondition.Negation negation when negation.child() instanceof SoqlCondition.Predicate ->
                    renderConjunction(List.of(negation), inheritedPatterns, unionBranch);
            case SoqlCondition.Negation negation -> "FILTER NOT EXISTS { " +
                    render(negation.child(), inheritedPatterns, false) + "} ";
        };
    }

    private String renderConjunction(List<SoqlCondition> children, Set<String> inheritedPatterns, boolean unionBranch) {
        final List<SoqlCondition> conditions = new ArrayList<>();
        children.forEach(child -> flattenConjunction(child, conditions));
        final StringBuilder result = new StringBuilder();
        final Set<String> localPatterns = new LinkedHashSet<>();
        final List<SoqlAttribute> filters = new ArrayList<>();
        final List<SoqlAttribute> negated = new ArrayList<>();
        for (SoqlCondition condition : conditions) {
            switch (condition) {
                case SoqlCondition.Predicate predicate -> {
                    final SoqlAttribute attribute = predicate.attribute();
                    for (String triple : triples(attribute)) {
                        if (!inheritedPatterns.contains(triple) && localPatterns.add(triple)) {
                            result.append(triple);
                        }
                    }
                    if (attribute.requiresFilter()) {
                        filters.add(attribute);
                    }
                }
                case SoqlCondition.Negation negation when negation.child() instanceof SoqlCondition.Predicate(
                        SoqlAttribute attribute
                ) -> negated.add(attribute);
                default -> {
                    final Set<String> visiblePatterns = new LinkedHashSet<>(inheritedPatterns);
                    visiblePatterns.addAll(localPatterns);
                    result.append(render(condition, visiblePatterns, unionBranch));
                }
            }
        }
        if (unionBranch && localPatterns.isEmpty() && !negated.isEmpty()) {
            result.append(String.join("", inheritedPatterns));
        }
        result.append(renderFilters(filters));
        if (!negated.isEmpty()) {
            result.append("FILTER NOT EXISTS { ");
            negated.forEach(attribute -> triples(attribute).forEach(result::append));
            result.append(renderFilters(negated.stream().filter(SoqlAttribute::requiresFilter).toList()));
            result.append("} ");
        }
        return result.toString();
    }

    private static void flattenConjunction(SoqlCondition condition, List<SoqlCondition> result) {
        if (condition instanceof SoqlCondition.Conjunction(List<SoqlCondition> children)) {
            children.forEach(child -> flattenConjunction(child, result));
        } else {
            result.add(condition);
        }
    }

    private String renderFilters(List<SoqlAttribute> attributes) {
        if (attributes.isEmpty()) {
            return "";
        }
        return "FILTER (" + attributes.stream()
                                      .flatMap(attribute -> attribute.getFilterExpressions(query.rootVariable())
                                                                     .stream())
                                      .collect(Collectors.joining(" && ")) + ") ";
    }

    private List<String> triples(SoqlAttribute attribute) {
        return attribute.getBasicGraphPattern(query.rootVariable(), query.rootIdentificationVariable(), triplePatternEnhancers);
    }
}
