/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenQueryParameter;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import org.antlr.v4.runtime.TokenStreamRewriter;

import java.util.List;

/**
 * Optimizes entity loading by modifying the query to fetch all named attributes.
 * <p>
 * This optimizer is applicable for SELECT queries that select instances of an entity class. Instead of loading the
 * instances one by one after the query is evaluated, this optimizer modifies the query to fetch all available entity
 * attributes by injecting optional triple patterns for each of the entity attributes and projecting the values from the
 * query. If the result type has subclasses, all attributes of the subclasses are enumerated in the query so that the
 * instance loading can then determine the result type and load the appropriate instance with all the relevant attribute
 * data.
 * <p>
 * The injected patterns look like this:
 * <pre>
 * OPTIONAL { ?subject &lt;property&gt; ?value }
 * </pre>
 * If a descriptor with at most one context (for each attribute) is provided, the injected patterns look like this:
 * <pre>
 * OPTIONAL { GRAPH ?g { ?subject &lt;property&gt; ?value } }
 * </pre>
 * Where {@literal ?g} is determined from the descriptor.
 * <p>
 * When <b>not</b> to use this modifier:
 * <ul>
 *     <li>When the result type has {@literal Properties} field</li>
 * </ul>
 */
public class AttributeEnumeratingSparqlAssemblyModifier implements SparqlAssemblyModifier {

    static final String TYPES_VAR_NAME = "types";
    static final String GROUP_CONCAT_SEPARATOR = "\u001F";

    private final MetamodelImpl metamodel;

    private final IdentifiableEntityType<?> resultType;

    private final Descriptor descriptor;

    private final EntityGraph<?> fetchGraph;

    private final boolean inferredAttsInDefault;

    private List<QueryVariableMapping> variableMapping;

    public AttributeEnumeratingSparqlAssemblyModifier(MetamodelImpl metamodel, IdentifiableEntityType<?> resultType,
                                                      Descriptor descriptor, EntityGraph<?> fetchGraph,
                                                      ConnectionWrapper connection) {
        this.metamodel = metamodel;
        this.resultType = resultType;
        this.descriptor = descriptor;
        this.fetchGraph = fetchGraph != null ? fetchGraph : generateDefaultFetchGraph();
        this.inferredAttsInDefault = resolveInferenceContext(connection);
        assert fetchGraph != null || resultType.getProperties() == null;
    }

    private static boolean resolveInferenceContext(ConnectionWrapper connection) {
        return "GraphDB".equals(connection.getRepositoryMetadata().getProductName());
    }

    private EntityGraph<?> generateDefaultFetchGraph() {
        return new FetchGraphProcessor(metamodel).generateDefaultFetchGraph(resultType);
    }

    @Override
    public void modify(TokenStreamSparqlQueryHolder queryHolder, TokenStreamRewriter tokenRewriter,
                       QueryAttributes queryAttributes) {
        assert queryAttributes.queryType() == QueryType.SELECT;
        assert queryHolder.getProjectedQueryParameters().size() == 1;

        final TokenQueryParameter<?> p = queryHolder.getProjectedQueryParameters().get(0);
        final String subjectParamName = UnboundPredicateObjectSparqlAssemblyModifier.getBaseParamName(p);
        final EntityMappingQueryModifier queryModifier = new EntityMappingQueryModifier(metamodel, resultType, descriptor, inferredAttsInDefault);
        final EntityMappingQueryModifier.QueryModification mod = queryModifier.modify(fetchGraph, subjectParamName);
        this.variableMapping = mod.variables();
        tokenRewriter.insertBefore(queryAttributes.lastClosingCurlyBraceToken(), mod.queryPart());
        modifyProjection(p, tokenRewriter, queryAttributes, mod);
    }

    private void modifyProjection(TokenQueryParameter<?> firstProjected, TokenStreamRewriter tokenRewriter,
                                  QueryAttributes queryAttributes,
                                  EntityMappingQueryModifier.QueryModification queryMod) {
        final StringBuilder projection = new StringBuilder();
        final StringBuilder groupBy = new StringBuilder(" GROUP BY " + firstProjected.getIdentifierAsQueryString());
        boolean hasGroupConcat = false;
        for (QueryVariableMapping varMapping : queryMod.variables()) {
            final String variable = "?" + varMapping.attributeVar();
            // PERF: GROUP_CONCAT prevents combinatorial blowup of the number of rows when properties have multiple values
            if (varMapping.canGroupConcat()) {
                projection.append(" (GROUP_CONCAT(DISTINCT ").append(variable).append("; SEPARATOR='")
                          .append(GROUP_CONCAT_SEPARATOR).append("') AS ").append(variable).append(')');
                hasGroupConcat = true;
            } else {
                projection.append(' ').append(variable);
                groupBy.append(' ').append(variable);
            }
        }
        tokenRewriter.insertAfter(firstProjected.getSingleToken(), projection.toString());
        if (hasGroupConcat) {
            tokenRewriter.insertAfter(queryAttributes.lastClosingCurlyBraceToken(), groupBy.toString());
        }
    }

    @Override
    public void accept(SparqlAssemblyModifierVisitor visitor) {
        visitor.visit(this);
    }

    public List<QueryVariableMapping> getVariableMapping() {
        return variableMapping;
    }
}
