package cz.cvut.kbss.jopa.query.soql;// Generated from soql.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class soqlParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		SELECT=1, WHERE=2, NOT=3, FROM=4, JOIN=5, AND=6, OR=7, ORDERBY=8, ORDERING=9, 
		GROUPBY=10, ASC=11, DESC=12, DISTINCT=13, QUERYOPERATOR=14, DOT=15, COMMA=16, 
		QMARK=17, COLON=18, TEXT=19, COLONTEXT=20, UPPERCASE=21, LOWERCASE=22, 
		DIGIT=23, NUMBER=24, VALUE=25, WHITESPACE=26;
	public static final int
		RULE_querySentence = 0, RULE_selectStatement = 1, RULE_typeDef = 2, RULE_params = 3, 
		RULE_paramComma = 4, RULE_distinctParam = 5, RULE_param = 6, RULE_objWithAttr = 7, 
		RULE_objWithOutAttr = 8, RULE_distinct = 9, RULE_object = 10, RULE_attribute = 11, 
		RULE_joinedParams = 12, RULE_tables = 13, RULE_table = 14, RULE_tableName = 15, 
		RULE_tableWithName = 16, RULE_logOp = 17, RULE_whereClausuleWrapper = 18, 
		RULE_whereClausules = 19, RULE_whereClausuleOps = 20, RULE_whereClausule = 21, 
		RULE_whereClausuleValue = 22, RULE_whereClausuleParam = 23, RULE_orderByClausule = 24, 
		RULE_orderByFullFormComma = 25, RULE_orderByFullForm = 26, RULE_orderByParam = 27, 
		RULE_groupByClausule = 28, RULE_groupByParamComma = 29, RULE_groupByParam = 30;
	public static final String[] ruleNames = {
		"querySentence", "selectStatement", "typeDef", "params", "paramComma", 
		"distinctParam", "param", "objWithAttr", "objWithOutAttr", "distinct", 
		"object", "attribute", "joinedParams", "tables", "table", "tableName", 
		"tableWithName", "logOp", "whereClausuleWrapper", "whereClausules", "whereClausuleOps", 
		"whereClausule", "whereClausuleValue", "whereClausuleParam", "orderByClausule", 
		"orderByFullFormComma", "orderByFullForm", "orderByParam", "groupByClausule", 
		"groupByParamComma", "groupByParam"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'SELECT'", "'WHERE'", "'NOT'", "'FROM'", "'JOIN'", "'AND'", "'OR'", 
		"'ORDER BY'", null, "'GROUP BY'", "'ASC'", "'DESC'", "'DISTINCT'", null, 
		"'.'", "','", "'\"'", "':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", 
		"ORDERING", "GROUPBY", "ASC", "DESC", "DISTINCT", "QUERYOPERATOR", "DOT", 
		"COMMA", "QMARK", "COLON", "TEXT", "COLONTEXT", "UPPERCASE", "LOWERCASE", 
		"DIGIT", "NUMBER", "VALUE", "WHITESPACE"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "soql.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public soqlParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class QuerySentenceContext extends ParserRuleContext {
		public SelectStatementContext selectStatement() {
			return getRuleContext(SelectStatementContext.class,0);
		}
		public WhereClausuleWrapperContext whereClausuleWrapper() {
			return getRuleContext(WhereClausuleWrapperContext.class,0);
		}
		public GroupByClausuleContext groupByClausule() {
			return getRuleContext(GroupByClausuleContext.class,0);
		}
		public OrderByClausuleContext orderByClausule() {
			return getRuleContext(OrderByClausuleContext.class,0);
		}
		public QuerySentenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_querySentence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterQuerySentence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitQuerySentence(this);
		}
	}

	public final QuerySentenceContext querySentence() throws RecognitionException {
		QuerySentenceContext _localctx = new QuerySentenceContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_querySentence);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(62);
			selectStatement();
			setState(64);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHERE) {
				{
				setState(63);
				whereClausuleWrapper();
				}
			}

			setState(67);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==GROUPBY) {
				{
				setState(66);
				groupByClausule();
				}
			}

			setState(70);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ORDERBY) {
				{
				setState(69);
				orderByClausule();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectStatementContext extends ParserRuleContext {
		public TypeDefContext typeDef() {
			return getRuleContext(TypeDefContext.class,0);
		}
		public ParamsContext params() {
			return getRuleContext(ParamsContext.class,0);
		}
		public TerminalNode FROM() { return getToken(soqlParser.FROM, 0); }
		public TablesContext tables() {
			return getRuleContext(TablesContext.class,0);
		}
		public SelectStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterSelectStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitSelectStatement(this);
		}
	}

	public final SelectStatementContext selectStatement() throws RecognitionException {
		SelectStatementContext _localctx = new SelectStatementContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_selectStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(72);
			typeDef();
			setState(73);
			params();
			setState(74);
			match(FROM);
			setState(75);
			tables();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeDefContext extends ParserRuleContext {
		public TerminalNode SELECT() { return getToken(soqlParser.SELECT, 0); }
		public TypeDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterTypeDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitTypeDef(this);
		}
	}

	public final TypeDefContext typeDef() throws RecognitionException {
		TypeDefContext _localctx = new TypeDefContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_typeDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(77);
			match(SELECT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamsContext extends ParserRuleContext {
		public DistinctParamContext distinctParam() {
			return getRuleContext(DistinctParamContext.class,0);
		}
		public List<ParamCommaContext> paramComma() {
			return getRuleContexts(ParamCommaContext.class);
		}
		public ParamCommaContext paramComma(int i) {
			return getRuleContext(ParamCommaContext.class,i);
		}
		public ParamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_params; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterParams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitParams(this);
		}
	}

	public final ParamsContext params() throws RecognitionException {
		ParamsContext _localctx = new ParamsContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_params);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(82);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(79);
					paramComma();
					}
					} 
				}
				setState(84);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			}
			setState(85);
			distinctParam();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamCommaContext extends ParserRuleContext {
		public DistinctParamContext distinctParam() {
			return getRuleContext(DistinctParamContext.class,0);
		}
		public TerminalNode COMMA() { return getToken(soqlParser.COMMA, 0); }
		public ParamCommaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramComma; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterParamComma(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitParamComma(this);
		}
	}

	public final ParamCommaContext paramComma() throws RecognitionException {
		ParamCommaContext _localctx = new ParamCommaContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_paramComma);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(87);
			distinctParam();
			setState(88);
			match(COMMA);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DistinctParamContext extends ParserRuleContext {
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
		}
		public DistinctContext distinct() {
			return getRuleContext(DistinctContext.class,0);
		}
		public DistinctParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_distinctParam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterDistinctParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitDistinctParam(this);
		}
	}

	public final DistinctParamContext distinctParam() throws RecognitionException {
		DistinctParamContext _localctx = new DistinctParamContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_distinctParam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(91);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DISTINCT) {
				{
				setState(90);
				distinct();
				}
			}

			setState(93);
			param();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamContext extends ParserRuleContext {
		public ObjWithAttrContext objWithAttr() {
			return getRuleContext(ObjWithAttrContext.class,0);
		}
		public ObjWithOutAttrContext objWithOutAttr() {
			return getRuleContext(ObjWithOutAttrContext.class,0);
		}
		public ParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_param; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitParam(this);
		}
	}

	public final ParamContext param() throws RecognitionException {
		ParamContext _localctx = new ParamContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_param);
		try {
			setState(97);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(95);
				objWithAttr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(96);
				objWithOutAttr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ObjWithAttrContext extends ParserRuleContext {
		public ObjectContext object() {
			return getRuleContext(ObjectContext.class,0);
		}
		public TerminalNode DOT() { return getToken(soqlParser.DOT, 0); }
		public AttributeContext attribute() {
			return getRuleContext(AttributeContext.class,0);
		}
		public ObjWithAttrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_objWithAttr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterObjWithAttr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitObjWithAttr(this);
		}
	}

	public final ObjWithAttrContext objWithAttr() throws RecognitionException {
		ObjWithAttrContext _localctx = new ObjWithAttrContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_objWithAttr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(99);
			object();
			setState(100);
			match(DOT);
			setState(101);
			attribute();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ObjWithOutAttrContext extends ParserRuleContext {
		public ObjectContext object() {
			return getRuleContext(ObjectContext.class,0);
		}
		public ObjWithOutAttrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_objWithOutAttr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterObjWithOutAttr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitObjWithOutAttr(this);
		}
	}

	public final ObjWithOutAttrContext objWithOutAttr() throws RecognitionException {
		ObjWithOutAttrContext _localctx = new ObjWithOutAttrContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_objWithOutAttr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(103);
			object();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DistinctContext extends ParserRuleContext {
		public TerminalNode DISTINCT() { return getToken(soqlParser.DISTINCT, 0); }
		public DistinctContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_distinct; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterDistinct(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitDistinct(this);
		}
	}

	public final DistinctContext distinct() throws RecognitionException {
		DistinctContext _localctx = new DistinctContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_distinct);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(105);
			match(DISTINCT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ObjectContext extends ParserRuleContext {
		public TerminalNode TEXT() { return getToken(soqlParser.TEXT, 0); }
		public ObjectContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_object; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterObject(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitObject(this);
		}
	}

	public final ObjectContext object() throws RecognitionException {
		ObjectContext _localctx = new ObjectContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_object);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(107);
			match(TEXT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeContext extends ParserRuleContext {
		public TerminalNode TEXT() { return getToken(soqlParser.TEXT, 0); }
		public AttributeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attribute; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterAttribute(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitAttribute(this);
		}
	}

	public final AttributeContext attribute() throws RecognitionException {
		AttributeContext _localctx = new AttributeContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_attribute);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(109);
			match(TEXT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class JoinedParamsContext extends ParserRuleContext {
		public ObjectContext object() {
			return getRuleContext(ObjectContext.class,0);
		}
		public List<TerminalNode> DOT() { return getTokens(soqlParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(soqlParser.DOT, i);
		}
		public List<AttributeContext> attribute() {
			return getRuleContexts(AttributeContext.class);
		}
		public AttributeContext attribute(int i) {
			return getRuleContext(AttributeContext.class,i);
		}
		public JoinedParamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_joinedParams; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterJoinedParams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitJoinedParams(this);
		}
	}

	public final JoinedParamsContext joinedParams() throws RecognitionException {
		JoinedParamsContext _localctx = new JoinedParamsContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_joinedParams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(111);
			object();
			setState(112);
			match(DOT);
			setState(113);
			attribute();
			setState(116); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(114);
				match(DOT);
				setState(115);
				attribute();
				}
				}
				setState(118); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==DOT );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TablesContext extends ParserRuleContext {
		public TableWithNameContext tableWithName() {
			return getRuleContext(TableWithNameContext.class,0);
		}
		public TablesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tables; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterTables(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitTables(this);
		}
	}

	public final TablesContext tables() throws RecognitionException {
		TablesContext _localctx = new TablesContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_tables);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(120);
			tableWithName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TableContext extends ParserRuleContext {
		public TerminalNode TEXT() { return getToken(soqlParser.TEXT, 0); }
		public TableContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_table; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterTable(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitTable(this);
		}
	}

	public final TableContext table() throws RecognitionException {
		TableContext _localctx = new TableContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_table);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(122);
			match(TEXT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TableNameContext extends ParserRuleContext {
		public TerminalNode TEXT() { return getToken(soqlParser.TEXT, 0); }
		public TableNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tableName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterTableName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitTableName(this);
		}
	}

	public final TableNameContext tableName() throws RecognitionException {
		TableNameContext _localctx = new TableNameContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_tableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(124);
			match(TEXT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TableWithNameContext extends ParserRuleContext {
		public TableContext table() {
			return getRuleContext(TableContext.class,0);
		}
		public TableNameContext tableName() {
			return getRuleContext(TableNameContext.class,0);
		}
		public TableWithNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tableWithName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterTableWithName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitTableWithName(this);
		}
	}

	public final TableWithNameContext tableWithName() throws RecognitionException {
		TableWithNameContext _localctx = new TableWithNameContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_tableWithName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(126);
			table();
			setState(127);
			tableName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LogOpContext extends ParserRuleContext {
		public TerminalNode AND() { return getToken(soqlParser.AND, 0); }
		public TerminalNode OR() { return getToken(soqlParser.OR, 0); }
		public LogOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterLogOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitLogOp(this);
		}
	}

	public final LogOpContext logOp() throws RecognitionException {
		LogOpContext _localctx = new LogOpContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_logOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(129);
			_la = _input.LA(1);
			if ( !(_la==AND || _la==OR) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhereClausuleWrapperContext extends ParserRuleContext {
		public TerminalNode WHERE() { return getToken(soqlParser.WHERE, 0); }
		public WhereClausulesContext whereClausules() {
			return getRuleContext(WhereClausulesContext.class,0);
		}
		public WhereClausuleWrapperContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausuleWrapper; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausuleWrapper(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausuleWrapper(this);
		}
	}

	public final WhereClausuleWrapperContext whereClausuleWrapper() throws RecognitionException {
		WhereClausuleWrapperContext _localctx = new WhereClausuleWrapperContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_whereClausuleWrapper);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(131);
			match(WHERE);
			setState(132);
			whereClausules();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhereClausulesContext extends ParserRuleContext {
		public List<WhereClausuleOpsContext> whereClausuleOps() {
			return getRuleContexts(WhereClausuleOpsContext.class);
		}
		public WhereClausuleOpsContext whereClausuleOps(int i) {
			return getRuleContext(WhereClausuleOpsContext.class,i);
		}
		public WhereClausulesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausules; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausules(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausules(this);
		}
	}

	public final WhereClausulesContext whereClausules() throws RecognitionException {
		WhereClausulesContext _localctx = new WhereClausulesContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_whereClausules);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(134);
			whereClausuleOps();
			setState(138);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << NOT) | (1L << AND) | (1L << OR) | (1L << TEXT))) != 0)) {
				{
				{
				setState(135);
				whereClausuleOps();
				}
				}
				setState(140);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhereClausuleOpsContext extends ParserRuleContext {
		public WhereClausuleContext whereClausule() {
			return getRuleContext(WhereClausuleContext.class,0);
		}
		public LogOpContext logOp() {
			return getRuleContext(LogOpContext.class,0);
		}
		public TerminalNode NOT() { return getToken(soqlParser.NOT, 0); }
		public WhereClausuleOpsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausuleOps; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausuleOps(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausuleOps(this);
		}
	}

	public final WhereClausuleOpsContext whereClausuleOps() throws RecognitionException {
		WhereClausuleOpsContext _localctx = new WhereClausuleOpsContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_whereClausuleOps);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(142);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==AND || _la==OR) {
				{
				setState(141);
				logOp();
				}
			}

			setState(145);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NOT) {
				{
				setState(144);
				match(NOT);
				}
			}

			setState(147);
			whereClausule();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhereClausuleContext extends ParserRuleContext {
		public WhereClausuleParamContext whereClausuleParam() {
			return getRuleContext(WhereClausuleParamContext.class,0);
		}
		public TerminalNode QUERYOPERATOR() { return getToken(soqlParser.QUERYOPERATOR, 0); }
		public WhereClausuleValueContext whereClausuleValue() {
			return getRuleContext(WhereClausuleValueContext.class,0);
		}
		public WhereClausuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausule(this);
		}
	}

	public final WhereClausuleContext whereClausule() throws RecognitionException {
		WhereClausuleContext _localctx = new WhereClausuleContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_whereClausule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(149);
			whereClausuleParam();
			setState(150);
			match(QUERYOPERATOR);
			setState(151);
			whereClausuleValue();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhereClausuleValueContext extends ParserRuleContext {
		public List<TerminalNode> QMARK() { return getTokens(soqlParser.QMARK); }
		public TerminalNode QMARK(int i) {
			return getToken(soqlParser.QMARK, i);
		}
		public TerminalNode TEXT() { return getToken(soqlParser.TEXT, 0); }
		public TerminalNode COLONTEXT() { return getToken(soqlParser.COLONTEXT, 0); }
		public WhereClausuleValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausuleValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausuleValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausuleValue(this);
		}
	}

	public final WhereClausuleValueContext whereClausuleValue() throws RecognitionException {
		WhereClausuleValueContext _localctx = new WhereClausuleValueContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_whereClausuleValue);
		try {
			setState(157);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case QMARK:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(153);
				match(QMARK);
				setState(154);
				match(TEXT);
				setState(155);
				match(QMARK);
				}
				}
				break;
			case COLONTEXT:
				enterOuterAlt(_localctx, 2);
				{
				setState(156);
				match(COLONTEXT);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhereClausuleParamContext extends ParserRuleContext {
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
		}
		public JoinedParamsContext joinedParams() {
			return getRuleContext(JoinedParamsContext.class,0);
		}
		public WhereClausuleParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausuleParam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausuleParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausuleParam(this);
		}
	}

	public final WhereClausuleParamContext whereClausuleParam() throws RecognitionException {
		WhereClausuleParamContext _localctx = new WhereClausuleParamContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_whereClausuleParam);
		try {
			setState(161);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,11,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(159);
				param();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(160);
				joinedParams();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderByClausuleContext extends ParserRuleContext {
		public TerminalNode ORDERBY() { return getToken(soqlParser.ORDERBY, 0); }
		public List<OrderByFullFormCommaContext> orderByFullFormComma() {
			return getRuleContexts(OrderByFullFormCommaContext.class);
		}
		public OrderByFullFormCommaContext orderByFullFormComma(int i) {
			return getRuleContext(OrderByFullFormCommaContext.class,i);
		}
		public OrderByClausuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderByClausule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterOrderByClausule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitOrderByClausule(this);
		}
	}

	public final OrderByClausuleContext orderByClausule() throws RecognitionException {
		OrderByClausuleContext _localctx = new OrderByClausuleContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_orderByClausule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(163);
			match(ORDERBY);
			setState(164);
			orderByFullFormComma();
			setState(168);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==TEXT) {
				{
				{
				setState(165);
				orderByFullFormComma();
				}
				}
				setState(170);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderByFullFormCommaContext extends ParserRuleContext {
		public OrderByFullFormContext orderByFullForm() {
			return getRuleContext(OrderByFullFormContext.class,0);
		}
		public TerminalNode COMMA() { return getToken(soqlParser.COMMA, 0); }
		public OrderByFullFormCommaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderByFullFormComma; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterOrderByFullFormComma(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitOrderByFullFormComma(this);
		}
	}

	public final OrderByFullFormCommaContext orderByFullFormComma() throws RecognitionException {
		OrderByFullFormCommaContext _localctx = new OrderByFullFormCommaContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_orderByFullFormComma);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(171);
			orderByFullForm();
			setState(173);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(172);
				match(COMMA);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderByFullFormContext extends ParserRuleContext {
		public OrderByParamContext orderByParam() {
			return getRuleContext(OrderByParamContext.class,0);
		}
		public TerminalNode ORDERING() { return getToken(soqlParser.ORDERING, 0); }
		public OrderByFullFormContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderByFullForm; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterOrderByFullForm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitOrderByFullForm(this);
		}
	}

	public final OrderByFullFormContext orderByFullForm() throws RecognitionException {
		OrderByFullFormContext _localctx = new OrderByFullFormContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_orderByFullForm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(175);
			orderByParam();
			setState(177);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ORDERING) {
				{
				setState(176);
				match(ORDERING);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OrderByParamContext extends ParserRuleContext {
		public ObjectContext object() {
			return getRuleContext(ObjectContext.class,0);
		}
		public List<TerminalNode> DOT() { return getTokens(soqlParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(soqlParser.DOT, i);
		}
		public List<AttributeContext> attribute() {
			return getRuleContexts(AttributeContext.class);
		}
		public AttributeContext attribute(int i) {
			return getRuleContext(AttributeContext.class,i);
		}
		public OrderByParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderByParam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterOrderByParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitOrderByParam(this);
		}
	}

	public final OrderByParamContext orderByParam() throws RecognitionException {
		OrderByParamContext _localctx = new OrderByParamContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_orderByParam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(179);
			object();
			setState(180);
			match(DOT);
			setState(181);
			attribute();
			setState(186);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DOT) {
				{
				{
				setState(182);
				match(DOT);
				setState(183);
				attribute();
				}
				}
				setState(188);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GroupByClausuleContext extends ParserRuleContext {
		public TerminalNode GROUPBY() { return getToken(soqlParser.GROUPBY, 0); }
		public List<GroupByParamCommaContext> groupByParamComma() {
			return getRuleContexts(GroupByParamCommaContext.class);
		}
		public GroupByParamCommaContext groupByParamComma(int i) {
			return getRuleContext(GroupByParamCommaContext.class,i);
		}
		public GroupByClausuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_groupByClausule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterGroupByClausule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitGroupByClausule(this);
		}
	}

	public final GroupByClausuleContext groupByClausule() throws RecognitionException {
		GroupByClausuleContext _localctx = new GroupByClausuleContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_groupByClausule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(189);
			match(GROUPBY);
			setState(190);
			groupByParamComma();
			setState(194);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==TEXT) {
				{
				{
				setState(191);
				groupByParamComma();
				}
				}
				setState(196);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GroupByParamCommaContext extends ParserRuleContext {
		public GroupByParamContext groupByParam() {
			return getRuleContext(GroupByParamContext.class,0);
		}
		public TerminalNode COMMA() { return getToken(soqlParser.COMMA, 0); }
		public GroupByParamCommaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_groupByParamComma; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterGroupByParamComma(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitGroupByParamComma(this);
		}
	}

	public final GroupByParamCommaContext groupByParamComma() throws RecognitionException {
		GroupByParamCommaContext _localctx = new GroupByParamCommaContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_groupByParamComma);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(197);
			groupByParam();
			setState(199);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(198);
				match(COMMA);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GroupByParamContext extends ParserRuleContext {
		public ObjectContext object() {
			return getRuleContext(ObjectContext.class,0);
		}
		public List<TerminalNode> DOT() { return getTokens(soqlParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(soqlParser.DOT, i);
		}
		public List<AttributeContext> attribute() {
			return getRuleContexts(AttributeContext.class);
		}
		public AttributeContext attribute(int i) {
			return getRuleContext(AttributeContext.class,i);
		}
		public GroupByParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_groupByParam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterGroupByParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitGroupByParam(this);
		}
	}

	public final GroupByParamContext groupByParam() throws RecognitionException {
		GroupByParamContext _localctx = new GroupByParamContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_groupByParam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(201);
			object();
			setState(202);
			match(DOT);
			setState(203);
			attribute();
			setState(208);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DOT) {
				{
				{
				setState(204);
				match(DOT);
				setState(205);
				attribute();
				}
				}
				setState(210);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\34\u00d6\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \3\2"+
		"\3\2\5\2C\n\2\3\2\5\2F\n\2\3\2\5\2I\n\2\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3"+
		"\5\7\5S\n\5\f\5\16\5V\13\5\3\5\3\5\3\6\3\6\3\6\3\7\5\7^\n\7\3\7\3\7\3"+
		"\b\3\b\5\bd\n\b\3\t\3\t\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16"+
		"\3\16\3\16\3\16\3\16\6\16w\n\16\r\16\16\16x\3\17\3\17\3\20\3\20\3\21\3"+
		"\21\3\22\3\22\3\22\3\23\3\23\3\24\3\24\3\24\3\25\3\25\7\25\u008b\n\25"+
		"\f\25\16\25\u008e\13\25\3\26\5\26\u0091\n\26\3\26\5\26\u0094\n\26\3\26"+
		"\3\26\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\5\30\u00a0\n\30\3\31\3\31"+
		"\5\31\u00a4\n\31\3\32\3\32\3\32\7\32\u00a9\n\32\f\32\16\32\u00ac\13\32"+
		"\3\33\3\33\5\33\u00b0\n\33\3\34\3\34\5\34\u00b4\n\34\3\35\3\35\3\35\3"+
		"\35\3\35\7\35\u00bb\n\35\f\35\16\35\u00be\13\35\3\36\3\36\3\36\7\36\u00c3"+
		"\n\36\f\36\16\36\u00c6\13\36\3\37\3\37\5\37\u00ca\n\37\3 \3 \3 \3 \3 "+
		"\7 \u00d1\n \f \16 \u00d4\13 \3 \2\2!\2\4\6\b\n\f\16\20\22\24\26\30\32"+
		"\34\36 \"$&(*,.\60\62\64\668:<>\2\3\3\2\b\t\2\u00c9\2@\3\2\2\2\4J\3\2"+
		"\2\2\6O\3\2\2\2\bT\3\2\2\2\nY\3\2\2\2\f]\3\2\2\2\16c\3\2\2\2\20e\3\2\2"+
		"\2\22i\3\2\2\2\24k\3\2\2\2\26m\3\2\2\2\30o\3\2\2\2\32q\3\2\2\2\34z\3\2"+
		"\2\2\36|\3\2\2\2 ~\3\2\2\2\"\u0080\3\2\2\2$\u0083\3\2\2\2&\u0085\3\2\2"+
		"\2(\u0088\3\2\2\2*\u0090\3\2\2\2,\u0097\3\2\2\2.\u009f\3\2\2\2\60\u00a3"+
		"\3\2\2\2\62\u00a5\3\2\2\2\64\u00ad\3\2\2\2\66\u00b1\3\2\2\28\u00b5\3\2"+
		"\2\2:\u00bf\3\2\2\2<\u00c7\3\2\2\2>\u00cb\3\2\2\2@B\5\4\3\2AC\5&\24\2"+
		"BA\3\2\2\2BC\3\2\2\2CE\3\2\2\2DF\5:\36\2ED\3\2\2\2EF\3\2\2\2FH\3\2\2\2"+
		"GI\5\62\32\2HG\3\2\2\2HI\3\2\2\2I\3\3\2\2\2JK\5\6\4\2KL\5\b\5\2LM\7\6"+
		"\2\2MN\5\34\17\2N\5\3\2\2\2OP\7\3\2\2P\7\3\2\2\2QS\5\n\6\2RQ\3\2\2\2S"+
		"V\3\2\2\2TR\3\2\2\2TU\3\2\2\2UW\3\2\2\2VT\3\2\2\2WX\5\f\7\2X\t\3\2\2\2"+
		"YZ\5\f\7\2Z[\7\22\2\2[\13\3\2\2\2\\^\5\24\13\2]\\\3\2\2\2]^\3\2\2\2^_"+
		"\3\2\2\2_`\5\16\b\2`\r\3\2\2\2ad\5\20\t\2bd\5\22\n\2ca\3\2\2\2cb\3\2\2"+
		"\2d\17\3\2\2\2ef\5\26\f\2fg\7\21\2\2gh\5\30\r\2h\21\3\2\2\2ij\5\26\f\2"+
		"j\23\3\2\2\2kl\7\17\2\2l\25\3\2\2\2mn\7\25\2\2n\27\3\2\2\2op\7\25\2\2"+
		"p\31\3\2\2\2qr\5\26\f\2rs\7\21\2\2sv\5\30\r\2tu\7\21\2\2uw\5\30\r\2vt"+
		"\3\2\2\2wx\3\2\2\2xv\3\2\2\2xy\3\2\2\2y\33\3\2\2\2z{\5\"\22\2{\35\3\2"+
		"\2\2|}\7\25\2\2}\37\3\2\2\2~\177\7\25\2\2\177!\3\2\2\2\u0080\u0081\5\36"+
		"\20\2\u0081\u0082\5 \21\2\u0082#\3\2\2\2\u0083\u0084\t\2\2\2\u0084%\3"+
		"\2\2\2\u0085\u0086\7\4\2\2\u0086\u0087\5(\25\2\u0087\'\3\2\2\2\u0088\u008c"+
		"\5*\26\2\u0089\u008b\5*\26\2\u008a\u0089\3\2\2\2\u008b\u008e\3\2\2\2\u008c"+
		"\u008a\3\2\2\2\u008c\u008d\3\2\2\2\u008d)\3\2\2\2\u008e\u008c\3\2\2\2"+
		"\u008f\u0091\5$\23\2\u0090\u008f\3\2\2\2\u0090\u0091\3\2\2\2\u0091\u0093"+
		"\3\2\2\2\u0092\u0094\7\5\2\2\u0093\u0092\3\2\2\2\u0093\u0094\3\2\2\2\u0094"+
		"\u0095\3\2\2\2\u0095\u0096\5,\27\2\u0096+\3\2\2\2\u0097\u0098\5\60\31"+
		"\2\u0098\u0099\7\20\2\2\u0099\u009a\5.\30\2\u009a-\3\2\2\2\u009b\u009c"+
		"\7\23\2\2\u009c\u009d\7\25\2\2\u009d\u00a0\7\23\2\2\u009e\u00a0\7\26\2"+
		"\2\u009f\u009b\3\2\2\2\u009f\u009e\3\2\2\2\u00a0/\3\2\2\2\u00a1\u00a4"+
		"\5\16\b\2\u00a2\u00a4\5\32\16\2\u00a3\u00a1\3\2\2\2\u00a3\u00a2\3\2\2"+
		"\2\u00a4\61\3\2\2\2\u00a5\u00a6\7\n\2\2\u00a6\u00aa\5\64\33\2\u00a7\u00a9"+
		"\5\64\33\2\u00a8\u00a7\3\2\2\2\u00a9\u00ac\3\2\2\2\u00aa\u00a8\3\2\2\2"+
		"\u00aa\u00ab\3\2\2\2\u00ab\63\3\2\2\2\u00ac\u00aa\3\2\2\2\u00ad\u00af"+
		"\5\66\34\2\u00ae\u00b0\7\22\2\2\u00af\u00ae\3\2\2\2\u00af\u00b0\3\2\2"+
		"\2\u00b0\65\3\2\2\2\u00b1\u00b3\58\35\2\u00b2\u00b4\7\13\2\2\u00b3\u00b2"+
		"\3\2\2\2\u00b3\u00b4\3\2\2\2\u00b4\67\3\2\2\2\u00b5\u00b6\5\26\f\2\u00b6"+
		"\u00b7\7\21\2\2\u00b7\u00bc\5\30\r\2\u00b8\u00b9\7\21\2\2\u00b9\u00bb"+
		"\5\30\r\2\u00ba\u00b8\3\2\2\2\u00bb\u00be\3\2\2\2\u00bc\u00ba\3\2\2\2"+
		"\u00bc\u00bd\3\2\2\2\u00bd9\3\2\2\2\u00be\u00bc\3\2\2\2\u00bf\u00c0\7"+
		"\f\2\2\u00c0\u00c4\5<\37\2\u00c1\u00c3\5<\37\2\u00c2\u00c1\3\2\2\2\u00c3"+
		"\u00c6\3\2\2\2\u00c4\u00c2\3\2\2\2\u00c4\u00c5\3\2\2\2\u00c5;\3\2\2\2"+
		"\u00c6\u00c4\3\2\2\2\u00c7\u00c9\5> \2\u00c8\u00ca\7\22\2\2\u00c9\u00c8"+
		"\3\2\2\2\u00c9\u00ca\3\2\2\2\u00ca=\3\2\2\2\u00cb\u00cc\5\26\f\2\u00cc"+
		"\u00cd\7\21\2\2\u00cd\u00d2\5\30\r\2\u00ce\u00cf\7\21\2\2\u00cf\u00d1"+
		"\5\30\r\2\u00d0\u00ce\3\2\2\2\u00d1\u00d4\3\2\2\2\u00d2\u00d0\3\2\2\2"+
		"\u00d2\u00d3\3\2\2\2\u00d3?\3\2\2\2\u00d4\u00d2\3\2\2\2\25BEHT]cx\u008c"+
		"\u0090\u0093\u009f\u00a3\u00aa\u00af\u00b3\u00bc\u00c4\u00c9\u00d2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}