package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.exception.QueryParserException;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import org.antlr.v4.runtime.Token;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Sparql11QueryListener extends SparqlParserBaseListener {

    private QueryType queryType;
    private final Map<Object, TokenQueryParameter<?>> parameters = new HashMap<>();

    private boolean inProjection;
    private boolean hasLimit;
    private boolean hasOffset;

    private int depth = 0;

    private Token lastClosingCurlyBrace;

    private final ParameterValueFactory parameterValueFactory;

    public Sparql11QueryListener(
            ParameterValueFactory parameterValueFactory) {this.parameterValueFactory = parameterValueFactory;}

    @Override
    public void enterSelectQuery(SparqlParser.SelectQueryContext ctx) {
        this.queryType = QueryType.SELECT;
    }

    @Override
    public void enterConstructQuery(SparqlParser.ConstructQueryContext ctx) {
        this.queryType = QueryType.CONSTRUCT;
    }

    @Override
    public void enterDescribeQuery(SparqlParser.DescribeQueryContext ctx) {
        super.enterDescribeQuery(ctx);
    }

    @Override
    public void enterAskQuery(SparqlParser.AskQueryContext ctx) {
        this.queryType = QueryType.ASK;
    }

    @Override
    public void enterInsertData(SparqlParser.InsertDataContext ctx) {
        this.queryType = QueryType.INSERT;
    }

    @Override
    public void enterInsertClause(SparqlParser.InsertClauseContext ctx) {
        this.queryType = QueryType.INSERT;
    }

    @Override
    public void enterDeleteData(SparqlParser.DeleteDataContext ctx) {
        this.queryType = QueryType.DELETE;
    }

    @Override
    public void enterDeleteWhere(SparqlParser.DeleteWhereContext ctx) {
        this.queryType = QueryType.DELETE;
    }

    @Override
    public void enterDeleteClause(SparqlParser.DeleteClauseContext ctx) {
        this.queryType = QueryType.DELETE;
    }

    @Override
    public void enterSelectVariables(SparqlParser.SelectVariablesContext ctx) {
        this.inProjection = true;
    }

    @Override
    public void exitSelectVariables(SparqlParser.SelectVariablesContext ctx) {
        this.inProjection = false;
    }

    @Override
    public void enterLimitClause(SparqlParser.LimitClauseContext ctx) {
        this.hasLimit = true;
    }

    @Override
    public void enterOffsetClause(SparqlParser.OffsetClauseContext ctx) {
        this.hasOffset = true;
    }

    @Override
    public void enterWhereClause(SparqlParser.WhereClauseContext ctx) {
        depth++;
    }

    @Override
    public void exitWhereClause(SparqlParser.WhereClauseContext ctx) {
        depth--;
        if (depth == 0) {
            lastClosingCurlyBrace = ctx.stop;
        }
    }

    @Override
    public void enterVar(SparqlParser.VarContext ctx) {
        final TokenQueryParameter<?> param;
        if (ctx.VAR1() != null) {
            param = parameters.computeIfAbsent(ctx.VAR1()
                                                  .getText()
                                                  .substring(1), k -> new TokenQueryParameter<>((String) k, parameterValueFactory));
        } else {
            assert ctx.VAR2() != null;
            try {
            final Integer position = Integer.parseInt(ctx.VAR2().getText().substring(1));
            if (parameters.containsKey(position)) {
                throw new QueryParserException("Parameter with position " + position + " already found in query " + ctx.getText());
            }
                param = new TokenQueryParameter<>(position, parameterValueFactory);
                parameters.put(position, param);
            } catch (NumberFormatException e) {
                throw new QueryParserException(ctx.VAR2().getText() + " is not a valid parameter position.", e);
            }
        }
        param.setProjected(param.isProjected() || inProjection);
        // There should be only one token representing the variable
        param.getTokens().add(ctx.getStart());
    }

    QueryAttributes getQueryAttributes() {
        return new QueryAttributes(queryType, hasOffset, hasLimit, lastClosingCurlyBrace);
    }

    List<TokenQueryParameter<?>> getParameters() {
        return new ArrayList<>(parameters.values());
    }
}
