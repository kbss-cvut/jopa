package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.query.QueryType;

import java.util.List;

record SoqlQueryModel(QueryType type, SelectProjection projection, String selectedVariable,
                      String rootVariable, String rootIdentificationVariable,
                      List<SoqlAttribute> requiredAttributes, SoqlCondition condition,
                      List<String> defaultGraphs, List<String> namedGraphs,
                      List<SoqlGroupParameter> grouping, List<SoqlOrderParameter> ordering) {

    SoqlQueryModel {
        requiredAttributes = List.copyOf(requiredAttributes);
        defaultGraphs = List.copyOf(defaultGraphs);
        namedGraphs = List.copyOf(namedGraphs);
        grouping = List.copyOf(grouping);
        ordering = List.copyOf(ordering);
    }
}
