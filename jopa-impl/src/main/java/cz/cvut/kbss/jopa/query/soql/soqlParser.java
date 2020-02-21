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
		GROUPBY=10, ASC=11, DESC=12, DISTINCT=13, COUNT=14, QUERYOPERATOR=15, 
		DOT=16, COMMA=17, QMARK=18, COLON=19, RIGHTPAREN=20, LEFTPAREN=21, TEXT=22, 
		COLONTEXT=23, UPPERCASE=24, LOWERCASE=25, DIGIT=26, NUMBER=27, VALUE=28, 
		WHITESPACE=29;
	public static final int
		RULE_querySentence = 0, RULE_selectStatement = 1, RULE_typeDef = 2, RULE_params = 3, 
		RULE_paramComma = 4, RULE_distinctParam = 5, RULE_selectedParam = 6, RULE_count = 7, 
		RULE_param = 8, RULE_objWithAttr = 9, RULE_objWithOutAttr = 10, RULE_distinct = 11, 
		RULE_object = 12, RULE_attribute = 13, RULE_joinedParams = 14, RULE_tables = 15, 
		RULE_table = 16, RULE_tableName = 17, RULE_tableWithName = 18, RULE_logOp = 19, 
		RULE_whereClausuleWrapper = 20, RULE_whereClausules = 21, RULE_whereClausuleOps = 22, 
		RULE_whereClausule = 23, RULE_whereClausuleValue = 24, RULE_whereClausuleParam = 25, 
		RULE_orderByClausule = 26, RULE_orderByFullFormComma = 27, RULE_orderByFullForm = 28, 
		RULE_orderByParam = 29, RULE_groupByClausule = 30, RULE_groupByParamComma = 31, 
		RULE_groupByParam = 32;
	public static final String[] ruleNames = {
		"querySentence", "selectStatement", "typeDef", "params", "paramComma", 
		"distinctParam", "selectedParam", "count", "param", "objWithAttr", "objWithOutAttr", 
		"distinct", "object", "attribute", "joinedParams", "tables", "table", 
		"tableName", "tableWithName", "logOp", "whereClausuleWrapper", "whereClausules", 
		"whereClausuleOps", "whereClausule", "whereClausuleValue", "whereClausuleParam", 
		"orderByClausule", "orderByFullFormComma", "orderByFullForm", "orderByParam", 
		"groupByClausule", "groupByParamComma", "groupByParam"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'SELECT'", "'WHERE'", "'NOT'", "'FROM'", "'JOIN'", "'AND'", "'OR'", 
		"'ORDER BY'", null, "'GROUP BY'", "'ASC'", "'DESC'", "'DISTINCT'", "'COUNT'", 
		null, "'.'", "','", "'\"'", "':'", "')'", "'('"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", 
		"ORDERING", "GROUPBY", "ASC", "DESC", "DISTINCT", "COUNT", "QUERYOPERATOR", 
		"DOT", "COMMA", "QMARK", "COLON", "RIGHTPAREN", "LEFTPAREN", "TEXT", "COLONTEXT", 
		"UPPERCASE", "LOWERCASE", "DIGIT", "NUMBER", "VALUE", "WHITESPACE"
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
			setState(66);
			selectStatement();
			setState(68);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHERE) {
				{
				setState(67);
				whereClausuleWrapper();
				}
			}

			setState(71);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==GROUPBY) {
				{
				setState(70);
				groupByClausule();
				}
			}

			setState(74);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ORDERBY) {
				{
				setState(73);
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
			setState(76);
			typeDef();
			setState(77);
			params();
			setState(78);
			match(FROM);
			setState(79);
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
			setState(81);
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
			setState(86);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(83);
					paramComma();
					}
					} 
				}
				setState(88);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			}
			setState(89);
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
			setState(91);
			distinctParam();
			setState(92);
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
		public SelectedParamContext selectedParam() {
			return getRuleContext(SelectedParamContext.class,0);
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
			setState(95);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DISTINCT) {
				{
				setState(94);
				distinct();
				}
			}

			setState(97);
			selectedParam();
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

	public static class SelectedParamContext extends ParserRuleContext {
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
		}
		public CountContext count() {
			return getRuleContext(CountContext.class,0);
		}
		public SelectedParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectedParam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterSelectedParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitSelectedParam(this);
		}
	}

	public final SelectedParamContext selectedParam() throws RecognitionException {
		SelectedParamContext _localctx = new SelectedParamContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_selectedParam);
		try {
			setState(101);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TEXT:
				enterOuterAlt(_localctx, 1);
				{
				setState(99);
				param();
				}
				break;
			case COUNT:
				enterOuterAlt(_localctx, 2);
				{
				setState(100);
				count();
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

	public static class CountContext extends ParserRuleContext {
		public TerminalNode COUNT() { return getToken(soqlParser.COUNT, 0); }
		public TerminalNode LEFTPAREN() { return getToken(soqlParser.LEFTPAREN, 0); }
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
		}
		public TerminalNode RIGHTPAREN() { return getToken(soqlParser.RIGHTPAREN, 0); }
		public CountContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_count; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterCount(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitCount(this);
		}
	}

	public final CountContext count() throws RecognitionException {
		CountContext _localctx = new CountContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_count);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(103);
			match(COUNT);
			setState(104);
			match(LEFTPAREN);
			setState(105);
			param();
			setState(106);
			match(RIGHTPAREN);
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
		enterRule(_localctx, 16, RULE_param);
		try {
			setState(110);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(108);
				objWithAttr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(109);
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
		enterRule(_localctx, 18, RULE_objWithAttr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(112);
			object();
			setState(113);
			match(DOT);
			setState(114);
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
		enterRule(_localctx, 20, RULE_objWithOutAttr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(116);
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
		enterRule(_localctx, 22, RULE_distinct);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(118);
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
		enterRule(_localctx, 24, RULE_object);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(120);
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
		enterRule(_localctx, 26, RULE_attribute);
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
		enterRule(_localctx, 28, RULE_joinedParams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(124);
			object();
			setState(125);
			match(DOT);
			setState(126);
			attribute();
			setState(129); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(127);
				match(DOT);
				setState(128);
				attribute();
				}
				}
				setState(131); 
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
		enterRule(_localctx, 30, RULE_tables);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(133);
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
		enterRule(_localctx, 32, RULE_table);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(135);
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
		enterRule(_localctx, 34, RULE_tableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(137);
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
		enterRule(_localctx, 36, RULE_tableWithName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(139);
			table();
			setState(140);
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
		enterRule(_localctx, 38, RULE_logOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(142);
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
		enterRule(_localctx, 40, RULE_whereClausuleWrapper);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(144);
			match(WHERE);
			setState(145);
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
		enterRule(_localctx, 42, RULE_whereClausules);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(147);
			whereClausuleOps();
			setState(151);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << NOT) | (1L << AND) | (1L << OR) | (1L << TEXT))) != 0)) {
				{
				{
				setState(148);
				whereClausuleOps();
				}
				}
				setState(153);
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
		enterRule(_localctx, 44, RULE_whereClausuleOps);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(155);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==AND || _la==OR) {
				{
				setState(154);
				logOp();
				}
			}

			setState(158);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NOT) {
				{
				setState(157);
				match(NOT);
				}
			}

			setState(160);
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
		enterRule(_localctx, 46, RULE_whereClausule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(162);
			whereClausuleParam();
			setState(163);
			match(QUERYOPERATOR);
			setState(164);
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
		enterRule(_localctx, 48, RULE_whereClausuleValue);
		try {
			setState(170);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case QMARK:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(166);
				match(QMARK);
				setState(167);
				match(TEXT);
				setState(168);
				match(QMARK);
				}
				}
				break;
			case COLONTEXT:
				enterOuterAlt(_localctx, 2);
				{
				setState(169);
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
		enterRule(_localctx, 50, RULE_whereClausuleParam);
		try {
			setState(174);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,12,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(172);
				param();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(173);
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
		enterRule(_localctx, 52, RULE_orderByClausule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(176);
			match(ORDERBY);
			setState(177);
			orderByFullFormComma();
			setState(181);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==TEXT) {
				{
				{
				setState(178);
				orderByFullFormComma();
				}
				}
				setState(183);
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
		enterRule(_localctx, 54, RULE_orderByFullFormComma);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(184);
			orderByFullForm();
			setState(186);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(185);
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
		enterRule(_localctx, 56, RULE_orderByFullForm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(188);
			orderByParam();
			setState(190);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ORDERING) {
				{
				setState(189);
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
		enterRule(_localctx, 58, RULE_orderByParam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(192);
			object();
			setState(193);
			match(DOT);
			setState(194);
			attribute();
			setState(199);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DOT) {
				{
				{
				setState(195);
				match(DOT);
				setState(196);
				attribute();
				}
				}
				setState(201);
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
		enterRule(_localctx, 60, RULE_groupByClausule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(202);
			match(GROUPBY);
			setState(203);
			groupByParamComma();
			setState(207);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==TEXT) {
				{
				{
				setState(204);
				groupByParamComma();
				}
				}
				setState(209);
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
		enterRule(_localctx, 62, RULE_groupByParamComma);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(210);
			groupByParam();
			setState(212);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(211);
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
		enterRule(_localctx, 64, RULE_groupByParam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(214);
			object();
			setState(215);
			match(DOT);
			setState(216);
			attribute();
			setState(221);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DOT) {
				{
				{
				setState(217);
				match(DOT);
				setState(218);
				attribute();
				}
				}
				setState(223);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\37\u00e3\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\3\2\3\2\5\2G\n\2\3\2\5\2J\n\2\3\2\5\2M\n\2\3\3\3\3\3\3\3\3"+
		"\3\3\3\4\3\4\3\5\7\5W\n\5\f\5\16\5Z\13\5\3\5\3\5\3\6\3\6\3\6\3\7\5\7b"+
		"\n\7\3\7\3\7\3\b\3\b\5\bh\n\b\3\t\3\t\3\t\3\t\3\t\3\n\3\n\5\nq\n\n\3\13"+
		"\3\13\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\20\3\20"+
		"\3\20\6\20\u0084\n\20\r\20\16\20\u0085\3\21\3\21\3\22\3\22\3\23\3\23\3"+
		"\24\3\24\3\24\3\25\3\25\3\26\3\26\3\26\3\27\3\27\7\27\u0098\n\27\f\27"+
		"\16\27\u009b\13\27\3\30\5\30\u009e\n\30\3\30\5\30\u00a1\n\30\3\30\3\30"+
		"\3\31\3\31\3\31\3\31\3\32\3\32\3\32\3\32\5\32\u00ad\n\32\3\33\3\33\5\33"+
		"\u00b1\n\33\3\34\3\34\3\34\7\34\u00b6\n\34\f\34\16\34\u00b9\13\34\3\35"+
		"\3\35\5\35\u00bd\n\35\3\36\3\36\5\36\u00c1\n\36\3\37\3\37\3\37\3\37\3"+
		"\37\7\37\u00c8\n\37\f\37\16\37\u00cb\13\37\3 \3 \3 \7 \u00d0\n \f \16"+
		" \u00d3\13 \3!\3!\5!\u00d7\n!\3\"\3\"\3\"\3\"\3\"\7\"\u00de\n\"\f\"\16"+
		"\"\u00e1\13\"\3\"\2\2#\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,"+
		".\60\62\64\668:<>@B\2\3\3\2\b\t\2\u00d5\2D\3\2\2\2\4N\3\2\2\2\6S\3\2\2"+
		"\2\bX\3\2\2\2\n]\3\2\2\2\fa\3\2\2\2\16g\3\2\2\2\20i\3\2\2\2\22p\3\2\2"+
		"\2\24r\3\2\2\2\26v\3\2\2\2\30x\3\2\2\2\32z\3\2\2\2\34|\3\2\2\2\36~\3\2"+
		"\2\2 \u0087\3\2\2\2\"\u0089\3\2\2\2$\u008b\3\2\2\2&\u008d\3\2\2\2(\u0090"+
		"\3\2\2\2*\u0092\3\2\2\2,\u0095\3\2\2\2.\u009d\3\2\2\2\60\u00a4\3\2\2\2"+
		"\62\u00ac\3\2\2\2\64\u00b0\3\2\2\2\66\u00b2\3\2\2\28\u00ba\3\2\2\2:\u00be"+
		"\3\2\2\2<\u00c2\3\2\2\2>\u00cc\3\2\2\2@\u00d4\3\2\2\2B\u00d8\3\2\2\2D"+
		"F\5\4\3\2EG\5*\26\2FE\3\2\2\2FG\3\2\2\2GI\3\2\2\2HJ\5> \2IH\3\2\2\2IJ"+
		"\3\2\2\2JL\3\2\2\2KM\5\66\34\2LK\3\2\2\2LM\3\2\2\2M\3\3\2\2\2NO\5\6\4"+
		"\2OP\5\b\5\2PQ\7\6\2\2QR\5 \21\2R\5\3\2\2\2ST\7\3\2\2T\7\3\2\2\2UW\5\n"+
		"\6\2VU\3\2\2\2WZ\3\2\2\2XV\3\2\2\2XY\3\2\2\2Y[\3\2\2\2ZX\3\2\2\2[\\\5"+
		"\f\7\2\\\t\3\2\2\2]^\5\f\7\2^_\7\23\2\2_\13\3\2\2\2`b\5\30\r\2a`\3\2\2"+
		"\2ab\3\2\2\2bc\3\2\2\2cd\5\16\b\2d\r\3\2\2\2eh\5\22\n\2fh\5\20\t\2ge\3"+
		"\2\2\2gf\3\2\2\2h\17\3\2\2\2ij\7\20\2\2jk\7\27\2\2kl\5\22\n\2lm\7\26\2"+
		"\2m\21\3\2\2\2nq\5\24\13\2oq\5\26\f\2pn\3\2\2\2po\3\2\2\2q\23\3\2\2\2"+
		"rs\5\32\16\2st\7\22\2\2tu\5\34\17\2u\25\3\2\2\2vw\5\32\16\2w\27\3\2\2"+
		"\2xy\7\17\2\2y\31\3\2\2\2z{\7\30\2\2{\33\3\2\2\2|}\7\30\2\2}\35\3\2\2"+
		"\2~\177\5\32\16\2\177\u0080\7\22\2\2\u0080\u0083\5\34\17\2\u0081\u0082"+
		"\7\22\2\2\u0082\u0084\5\34\17\2\u0083\u0081\3\2\2\2\u0084\u0085\3\2\2"+
		"\2\u0085\u0083\3\2\2\2\u0085\u0086\3\2\2\2\u0086\37\3\2\2\2\u0087\u0088"+
		"\5&\24\2\u0088!\3\2\2\2\u0089\u008a\7\30\2\2\u008a#\3\2\2\2\u008b\u008c"+
		"\7\30\2\2\u008c%\3\2\2\2\u008d\u008e\5\"\22\2\u008e\u008f\5$\23\2\u008f"+
		"\'\3\2\2\2\u0090\u0091\t\2\2\2\u0091)\3\2\2\2\u0092\u0093\7\4\2\2\u0093"+
		"\u0094\5,\27\2\u0094+\3\2\2\2\u0095\u0099\5.\30\2\u0096\u0098\5.\30\2"+
		"\u0097\u0096\3\2\2\2\u0098\u009b\3\2\2\2\u0099\u0097\3\2\2\2\u0099\u009a"+
		"\3\2\2\2\u009a-\3\2\2\2\u009b\u0099\3\2\2\2\u009c\u009e\5(\25\2\u009d"+
		"\u009c\3\2\2\2\u009d\u009e\3\2\2\2\u009e\u00a0\3\2\2\2\u009f\u00a1\7\5"+
		"\2\2\u00a0\u009f\3\2\2\2\u00a0\u00a1\3\2\2\2\u00a1\u00a2\3\2\2\2\u00a2"+
		"\u00a3\5\60\31\2\u00a3/\3\2\2\2\u00a4\u00a5\5\64\33\2\u00a5\u00a6\7\21"+
		"\2\2\u00a6\u00a7\5\62\32\2\u00a7\61\3\2\2\2\u00a8\u00a9\7\24\2\2\u00a9"+
		"\u00aa\7\30\2\2\u00aa\u00ad\7\24\2\2\u00ab\u00ad\7\31\2\2\u00ac\u00a8"+
		"\3\2\2\2\u00ac\u00ab\3\2\2\2\u00ad\63\3\2\2\2\u00ae\u00b1\5\22\n\2\u00af"+
		"\u00b1\5\36\20\2\u00b0\u00ae\3\2\2\2\u00b0\u00af\3\2\2\2\u00b1\65\3\2"+
		"\2\2\u00b2\u00b3\7\n\2\2\u00b3\u00b7\58\35\2\u00b4\u00b6\58\35\2\u00b5"+
		"\u00b4\3\2\2\2\u00b6\u00b9\3\2\2\2\u00b7\u00b5\3\2\2\2\u00b7\u00b8\3\2"+
		"\2\2\u00b8\67\3\2\2\2\u00b9\u00b7\3\2\2\2\u00ba\u00bc\5:\36\2\u00bb\u00bd"+
		"\7\23\2\2\u00bc\u00bb\3\2\2\2\u00bc\u00bd\3\2\2\2\u00bd9\3\2\2\2\u00be"+
		"\u00c0\5<\37\2\u00bf\u00c1\7\13\2\2\u00c0\u00bf\3\2\2\2\u00c0\u00c1\3"+
		"\2\2\2\u00c1;\3\2\2\2\u00c2\u00c3\5\32\16\2\u00c3\u00c4\7\22\2\2\u00c4"+
		"\u00c9\5\34\17\2\u00c5\u00c6\7\22\2\2\u00c6\u00c8\5\34\17\2\u00c7\u00c5"+
		"\3\2\2\2\u00c8\u00cb\3\2\2\2\u00c9\u00c7\3\2\2\2\u00c9\u00ca\3\2\2\2\u00ca"+
		"=\3\2\2\2\u00cb\u00c9\3\2\2\2\u00cc\u00cd\7\f\2\2\u00cd\u00d1\5@!\2\u00ce"+
		"\u00d0\5@!\2\u00cf\u00ce\3\2\2\2\u00d0\u00d3\3\2\2\2\u00d1\u00cf\3\2\2"+
		"\2\u00d1\u00d2\3\2\2\2\u00d2?\3\2\2\2\u00d3\u00d1\3\2\2\2\u00d4\u00d6"+
		"\5B\"\2\u00d5\u00d7\7\23\2\2\u00d6\u00d5\3\2\2\2\u00d6\u00d7\3\2\2\2\u00d7"+
		"A\3\2\2\2\u00d8\u00d9\5\32\16\2\u00d9\u00da\7\22\2\2\u00da\u00df\5\34"+
		"\17\2\u00db\u00dc\7\22\2\2\u00dc\u00de\5\34\17\2\u00dd\u00db\3\2\2\2\u00de"+
		"\u00e1\3\2\2\2\u00df\u00dd\3\2\2\2\u00df\u00e0\3\2\2\2\u00e0C\3\2\2\2"+
		"\u00e1\u00df\3\2\2\2\26FILXagp\u0085\u0099\u009d\u00a0\u00ac\u00b0\u00b7"+
		"\u00bc\u00c0\u00c9\u00d1\u00d6\u00df";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}