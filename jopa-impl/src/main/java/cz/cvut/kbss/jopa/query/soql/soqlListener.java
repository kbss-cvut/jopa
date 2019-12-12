package cz.cvut.kbss.jopa.query.soql;// Generated from soql.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link soqlParser}.
 */
public interface soqlListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link soqlParser#querySentence}.
	 * @param ctx the parse tree
	 */
	void enterQuerySentence(soqlParser.QuerySentenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#querySentence}.
	 * @param ctx the parse tree
	 */
	void exitQuerySentence(soqlParser.QuerySentenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#selectStatement}.
	 * @param ctx the parse tree
	 */
	void enterSelectStatement(soqlParser.SelectStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#selectStatement}.
	 * @param ctx the parse tree
	 */
	void exitSelectStatement(soqlParser.SelectStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#typeDef}.
	 * @param ctx the parse tree
	 */
	void enterTypeDef(soqlParser.TypeDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#typeDef}.
	 * @param ctx the parse tree
	 */
	void exitTypeDef(soqlParser.TypeDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#params}.
	 * @param ctx the parse tree
	 */
	void enterParams(soqlParser.ParamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#params}.
	 * @param ctx the parse tree
	 */
	void exitParams(soqlParser.ParamsContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#paramComma}.
	 * @param ctx the parse tree
	 */
	void enterParamComma(soqlParser.ParamCommaContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#paramComma}.
	 * @param ctx the parse tree
	 */
	void exitParamComma(soqlParser.ParamCommaContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#distinctParam}.
	 * @param ctx the parse tree
	 */
	void enterDistinctParam(soqlParser.DistinctParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#distinctParam}.
	 * @param ctx the parse tree
	 */
	void exitDistinctParam(soqlParser.DistinctParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#param}.
	 * @param ctx the parse tree
	 */
	void enterParam(soqlParser.ParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#param}.
	 * @param ctx the parse tree
	 */
	void exitParam(soqlParser.ParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#objWithAttr}.
	 * @param ctx the parse tree
	 */
	void enterObjWithAttr(soqlParser.ObjWithAttrContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#objWithAttr}.
	 * @param ctx the parse tree
	 */
	void exitObjWithAttr(soqlParser.ObjWithAttrContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#objWithOutAttr}.
	 * @param ctx the parse tree
	 */
	void enterObjWithOutAttr(soqlParser.ObjWithOutAttrContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#objWithOutAttr}.
	 * @param ctx the parse tree
	 */
	void exitObjWithOutAttr(soqlParser.ObjWithOutAttrContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#distinct}.
	 * @param ctx the parse tree
	 */
	void enterDistinct(soqlParser.DistinctContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#distinct}.
	 * @param ctx the parse tree
	 */
	void exitDistinct(soqlParser.DistinctContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#object}.
	 * @param ctx the parse tree
	 */
	void enterObject(soqlParser.ObjectContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#object}.
	 * @param ctx the parse tree
	 */
	void exitObject(soqlParser.ObjectContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#attribute}.
	 * @param ctx the parse tree
	 */
	void enterAttribute(soqlParser.AttributeContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#attribute}.
	 * @param ctx the parse tree
	 */
	void exitAttribute(soqlParser.AttributeContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#joinedParams}.
	 * @param ctx the parse tree
	 */
	void enterJoinedParams(soqlParser.JoinedParamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#joinedParams}.
	 * @param ctx the parse tree
	 */
	void exitJoinedParams(soqlParser.JoinedParamsContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#tables}.
	 * @param ctx the parse tree
	 */
	void enterTables(soqlParser.TablesContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#tables}.
	 * @param ctx the parse tree
	 */
	void exitTables(soqlParser.TablesContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#table}.
	 * @param ctx the parse tree
	 */
	void enterTable(soqlParser.TableContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#table}.
	 * @param ctx the parse tree
	 */
	void exitTable(soqlParser.TableContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#tableName}.
	 * @param ctx the parse tree
	 */
	void enterTableName(soqlParser.TableNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#tableName}.
	 * @param ctx the parse tree
	 */
	void exitTableName(soqlParser.TableNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#tableWithName}.
	 * @param ctx the parse tree
	 */
	void enterTableWithName(soqlParser.TableWithNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#tableWithName}.
	 * @param ctx the parse tree
	 */
	void exitTableWithName(soqlParser.TableWithNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#logOp}.
	 * @param ctx the parse tree
	 */
	void enterLogOp(soqlParser.LogOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#logOp}.
	 * @param ctx the parse tree
	 */
	void exitLogOp(soqlParser.LogOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#whereClausuleWrapper}.
	 * @param ctx the parse tree
	 */
	void enterWhereClausuleWrapper(soqlParser.WhereClausuleWrapperContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#whereClausuleWrapper}.
	 * @param ctx the parse tree
	 */
	void exitWhereClausuleWrapper(soqlParser.WhereClausuleWrapperContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#whereClausules}.
	 * @param ctx the parse tree
	 */
	void enterWhereClausules(soqlParser.WhereClausulesContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#whereClausules}.
	 * @param ctx the parse tree
	 */
	void exitWhereClausules(soqlParser.WhereClausulesContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#whereClausuleOps}.
	 * @param ctx the parse tree
	 */
	void enterWhereClausuleOps(soqlParser.WhereClausuleOpsContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#whereClausuleOps}.
	 * @param ctx the parse tree
	 */
	void exitWhereClausuleOps(soqlParser.WhereClausuleOpsContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#whereClausule}.
	 * @param ctx the parse tree
	 */
	void enterWhereClausule(soqlParser.WhereClausuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#whereClausule}.
	 * @param ctx the parse tree
	 */
	void exitWhereClausule(soqlParser.WhereClausuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#whereClausuleValue}.
	 * @param ctx the parse tree
	 */
	void enterWhereClausuleValue(soqlParser.WhereClausuleValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#whereClausuleValue}.
	 * @param ctx the parse tree
	 */
	void exitWhereClausuleValue(soqlParser.WhereClausuleValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#whereClausuleParam}.
	 * @param ctx the parse tree
	 */
	void enterWhereClausuleParam(soqlParser.WhereClausuleParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#whereClausuleParam}.
	 * @param ctx the parse tree
	 */
	void exitWhereClausuleParam(soqlParser.WhereClausuleParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#orderByClausule}.
	 * @param ctx the parse tree
	 */
	void enterOrderByClausule(soqlParser.OrderByClausuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#orderByClausule}.
	 * @param ctx the parse tree
	 */
	void exitOrderByClausule(soqlParser.OrderByClausuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#orderByFullFormComma}.
	 * @param ctx the parse tree
	 */
	void enterOrderByFullFormComma(soqlParser.OrderByFullFormCommaContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#orderByFullFormComma}.
	 * @param ctx the parse tree
	 */
	void exitOrderByFullFormComma(soqlParser.OrderByFullFormCommaContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#orderByFullForm}.
	 * @param ctx the parse tree
	 */
	void enterOrderByFullForm(soqlParser.OrderByFullFormContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#orderByFullForm}.
	 * @param ctx the parse tree
	 */
	void exitOrderByFullForm(soqlParser.OrderByFullFormContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#orderByParam}.
	 * @param ctx the parse tree
	 */
	void enterOrderByParam(soqlParser.OrderByParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#orderByParam}.
	 * @param ctx the parse tree
	 */
	void exitOrderByParam(soqlParser.OrderByParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#groupByClausule}.
	 * @param ctx the parse tree
	 */
	void enterGroupByClausule(soqlParser.GroupByClausuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#groupByClausule}.
	 * @param ctx the parse tree
	 */
	void exitGroupByClausule(soqlParser.GroupByClausuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#groupByParamComma}.
	 * @param ctx the parse tree
	 */
	void enterGroupByParamComma(soqlParser.GroupByParamCommaContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#groupByParamComma}.
	 * @param ctx the parse tree
	 */
	void exitGroupByParamComma(soqlParser.GroupByParamCommaContext ctx);
	/**
	 * Enter a parse tree produced by {@link soqlParser#groupByParam}.
	 * @param ctx the parse tree
	 */
	void enterGroupByParam(soqlParser.GroupByParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link soqlParser#groupByParam}.
	 * @param ctx the parse tree
	 */
	void exitGroupByParam(soqlParser.GroupByParamContext ctx);
}