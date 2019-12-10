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
		ASC=10, DESC=11, DISTINCT=12, LEFTOUTERJOIN=13, QUERYOPERATOR=14, RIGHTPAREN=15, 
		LEFTPAREN=16, DOT=17, COMMA=18, QMARK=19, COLON=20, TEXT=21, COLONTEXT=22, 
		UPPERCASE=23, LOWERCASE=24, DIGIT=25, NUMBER=26, VALUE=27, WHITESPACE=28;
	public static final int
		RULE_querySentence = 0, RULE_params = 1, RULE_param = 2, RULE_joinedParams = 3, 
		RULE_paramComma = 4, RULE_object = 5, RULE_objWithAttr = 6, RULE_objWithOutAttr = 7, 
		RULE_attribute = 8, RULE_typeDef = 9, RULE_distinct = 10, RULE_logOp = 11, 
		RULE_tables = 12, RULE_table = 13, RULE_tableName = 14, RULE_tableWithName = 15, 
		RULE_whereClausules = 16, RULE_whereClausuleNot = 17, RULE_whereClausule = 18, 
		RULE_whereClausuleJoin = 19, RULE_whereClausuleValue = 20, RULE_clausuleJoinNot = 21, 
		RULE_clausuleJoin = 22, RULE_orderByClausule = 23, RULE_orderBySingleComma = 24, 
		RULE_orderBySingle = 25, RULE_orderByParam = 26;
	public static final String[] ruleNames = {
		"querySentence", "params", "param", "joinedParams", "paramComma", "object", 
		"objWithAttr", "objWithOutAttr", "attribute", "typeDef", "distinct", "logOp", 
		"tables", "table", "tableName", "tableWithName", "whereClausules", "whereClausuleNot", 
		"whereClausule", "whereClausuleJoin", "whereClausuleValue", "clausuleJoinNot", 
		"clausuleJoin", "orderByClausule", "orderBySingleComma", "orderBySingle", 
		"orderByParam"
	};

	private static final String[] _LITERAL_NAMES = {
		null, null, null, null, null, null, null, null, "'ORDER BY'", null, "'ASC'", 
		"'DESC'", "'DISTINCT'", null, null, "')'", "'('", "'.'", "','", "'\"'", 
		"':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", 
		"ORDERING", "ASC", "DESC", "DISTINCT", "LEFTOUTERJOIN", "QUERYOPERATOR", 
		"RIGHTPAREN", "LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", "TEXT", "COLONTEXT", 
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
		public DistinctContext distinct() {
			return getRuleContext(DistinctContext.class,0);
		}
		public TerminalNode WHERE() { return getToken(soqlParser.WHERE, 0); }
		public WhereClausuleJoinContext whereClausuleJoin() {
			return getRuleContext(WhereClausuleJoinContext.class,0);
		}
		public WhereClausulesContext whereClausules() {
			return getRuleContext(WhereClausulesContext.class,0);
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
			setState(54);
			typeDef();
			setState(56);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DISTINCT) {
				{
				setState(55);
				distinct();
				}
			}

			setState(58);
			params();
			setState(59);
			match(FROM);
			setState(60);
			tables();
			setState(62);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHERE) {
				{
				setState(61);
				match(WHERE);
				}
			}

			setState(65);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				{
				setState(64);
				whereClausuleJoin();
				}
				break;
			}
			setState(68);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << NOT) | (1L << AND) | (1L << OR) | (1L << TEXT))) != 0)) {
				{
				setState(67);
				whereClausules();
				}
			}

			setState(71);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ORDERBY) {
				{
				setState(70);
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

	public static class ParamsContext extends ParserRuleContext {
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
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
		enterRule(_localctx, 2, RULE_params);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(76);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(73);
					paramComma();
					}
					} 
				}
				setState(78);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			}
			setState(79);
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
		enterRule(_localctx, 4, RULE_param);
		try {
			setState(83);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(81);
				objWithAttr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(82);
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
		enterRule(_localctx, 6, RULE_joinedParams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(85);
			object();
			setState(86);
			match(DOT);
			setState(87);
			attribute();
			setState(90); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(88);
				match(DOT);
				setState(89);
				attribute();
				}
				}
				setState(92); 
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

	public static class ParamCommaContext extends ParserRuleContext {
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
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
			setState(94);
			param();
			setState(95);
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
		enterRule(_localctx, 10, RULE_object);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(97);
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
		enterRule(_localctx, 12, RULE_objWithAttr);
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
		enterRule(_localctx, 14, RULE_objWithOutAttr);
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
		enterRule(_localctx, 16, RULE_attribute);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(105);
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
		enterRule(_localctx, 18, RULE_typeDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(107);
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
		enterRule(_localctx, 20, RULE_distinct);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(109);
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
		enterRule(_localctx, 22, RULE_logOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(111);
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
		enterRule(_localctx, 24, RULE_tables);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(113);
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
		enterRule(_localctx, 26, RULE_table);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(115);
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
		enterRule(_localctx, 28, RULE_tableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(117);
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
		enterRule(_localctx, 30, RULE_tableWithName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			table();
			setState(120);
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

	public static class WhereClausulesContext extends ParserRuleContext {
		public List<WhereClausuleNotContext> whereClausuleNot() {
			return getRuleContexts(WhereClausuleNotContext.class);
		}
		public WhereClausuleNotContext whereClausuleNot(int i) {
			return getRuleContext(WhereClausuleNotContext.class,i);
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
		enterRule(_localctx, 32, RULE_whereClausules);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(122);
			whereClausuleNot();
			setState(126);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << NOT) | (1L << AND) | (1L << OR) | (1L << TEXT))) != 0)) {
				{
				{
				setState(123);
				whereClausuleNot();
				}
				}
				setState(128);
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

	public static class WhereClausuleNotContext extends ParserRuleContext {
		public WhereClausuleContext whereClausule() {
			return getRuleContext(WhereClausuleContext.class,0);
		}
		public LogOpContext logOp() {
			return getRuleContext(LogOpContext.class,0);
		}
		public TerminalNode NOT() { return getToken(soqlParser.NOT, 0); }
		public WhereClausuleNotContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausuleNot; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausuleNot(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausuleNot(this);
		}
	}

	public final WhereClausuleNotContext whereClausuleNot() throws RecognitionException {
		WhereClausuleNotContext _localctx = new WhereClausuleNotContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_whereClausuleNot);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(130);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==AND || _la==OR) {
				{
				setState(129);
				logOp();
				}
			}

			setState(133);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NOT) {
				{
				setState(132);
				match(NOT);
				}
			}

			setState(135);
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
		public ParamContext param() {
			return getRuleContext(ParamContext.class,0);
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
		enterRule(_localctx, 36, RULE_whereClausule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(137);
			param();
			setState(138);
			match(QUERYOPERATOR);
			setState(139);
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

	public static class WhereClausuleJoinContext extends ParserRuleContext {
		public List<ClausuleJoinNotContext> clausuleJoinNot() {
			return getRuleContexts(ClausuleJoinNotContext.class);
		}
		public ClausuleJoinNotContext clausuleJoinNot(int i) {
			return getRuleContext(ClausuleJoinNotContext.class,i);
		}
		public WhereClausuleJoinContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereClausuleJoin; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterWhereClausuleJoin(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitWhereClausuleJoin(this);
		}
	}

	public final WhereClausuleJoinContext whereClausuleJoin() throws RecognitionException {
		WhereClausuleJoinContext _localctx = new WhereClausuleJoinContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_whereClausuleJoin);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(141);
			clausuleJoinNot();
			setState(145);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(142);
					clausuleJoinNot();
					}
					} 
				}
				setState(147);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
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
		enterRule(_localctx, 40, RULE_whereClausuleValue);
		try {
			setState(152);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case QMARK:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(148);
				match(QMARK);
				setState(149);
				match(TEXT);
				setState(150);
				match(QMARK);
				}
				}
				break;
			case COLONTEXT:
				enterOuterAlt(_localctx, 2);
				{
				setState(151);
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

	public static class ClausuleJoinNotContext extends ParserRuleContext {
		public ClausuleJoinContext clausuleJoin() {
			return getRuleContext(ClausuleJoinContext.class,0);
		}
		public LogOpContext logOp() {
			return getRuleContext(LogOpContext.class,0);
		}
		public TerminalNode NOT() { return getToken(soqlParser.NOT, 0); }
		public ClausuleJoinNotContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_clausuleJoinNot; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterClausuleJoinNot(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitClausuleJoinNot(this);
		}
	}

	public final ClausuleJoinNotContext clausuleJoinNot() throws RecognitionException {
		ClausuleJoinNotContext _localctx = new ClausuleJoinNotContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_clausuleJoinNot);
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
			clausuleJoin();
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

	public static class ClausuleJoinContext extends ParserRuleContext {
		public JoinedParamsContext joinedParams() {
			return getRuleContext(JoinedParamsContext.class,0);
		}
		public TerminalNode QUERYOPERATOR() { return getToken(soqlParser.QUERYOPERATOR, 0); }
		public WhereClausuleValueContext whereClausuleValue() {
			return getRuleContext(WhereClausuleValueContext.class,0);
		}
		public ClausuleJoinContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_clausuleJoin; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterClausuleJoin(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitClausuleJoin(this);
		}
	}

	public final ClausuleJoinContext clausuleJoin() throws RecognitionException {
		ClausuleJoinContext _localctx = new ClausuleJoinContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_clausuleJoin);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(162);
			joinedParams();
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

	public static class OrderByClausuleContext extends ParserRuleContext {
		public TerminalNode ORDERBY() { return getToken(soqlParser.ORDERBY, 0); }
		public List<OrderBySingleCommaContext> orderBySingleComma() {
			return getRuleContexts(OrderBySingleCommaContext.class);
		}
		public OrderBySingleCommaContext orderBySingleComma(int i) {
			return getRuleContext(OrderBySingleCommaContext.class,i);
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
		enterRule(_localctx, 46, RULE_orderByClausule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(166);
			match(ORDERBY);
			setState(167);
			orderBySingleComma();
			setState(171);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==TEXT) {
				{
				{
				setState(168);
				orderBySingleComma();
				}
				}
				setState(173);
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

	public static class OrderBySingleCommaContext extends ParserRuleContext {
		public OrderBySingleContext orderBySingle() {
			return getRuleContext(OrderBySingleContext.class,0);
		}
		public TerminalNode COMMA() { return getToken(soqlParser.COMMA, 0); }
		public OrderBySingleCommaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderBySingleComma; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterOrderBySingleComma(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitOrderBySingleComma(this);
		}
	}

	public final OrderBySingleCommaContext orderBySingleComma() throws RecognitionException {
		OrderBySingleCommaContext _localctx = new OrderBySingleCommaContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_orderBySingleComma);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(174);
			orderBySingle();
			setState(176);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(175);
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

	public static class OrderBySingleContext extends ParserRuleContext {
		public OrderByParamContext orderByParam() {
			return getRuleContext(OrderByParamContext.class,0);
		}
		public TerminalNode ORDERING() { return getToken(soqlParser.ORDERING, 0); }
		public OrderBySingleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orderBySingle; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).enterOrderBySingle(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof soqlListener ) ((soqlListener)listener).exitOrderBySingle(this);
		}
	}

	public final OrderBySingleContext orderBySingle() throws RecognitionException {
		OrderBySingleContext _localctx = new OrderBySingleContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_orderBySingle);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(178);
			orderByParam();
			setState(180);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ORDERING) {
				{
				setState(179);
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
		enterRule(_localctx, 52, RULE_orderByParam);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(182);
			object();
			setState(183);
			match(DOT);
			setState(184);
			attribute();
			setState(189);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DOT) {
				{
				{
				setState(185);
				match(DOT);
				setState(186);
				attribute();
				}
				}
				setState(191);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\36\u00c3\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\3\2\3\2\5\2;\n\2\3\2\3\2\3\2\3\2\5\2A\n"+
		"\2\3\2\5\2D\n\2\3\2\5\2G\n\2\3\2\5\2J\n\2\3\3\7\3M\n\3\f\3\16\3P\13\3"+
		"\3\3\3\3\3\4\3\4\5\4V\n\4\3\5\3\5\3\5\3\5\3\5\6\5]\n\5\r\5\16\5^\3\6\3"+
		"\6\3\6\3\7\3\7\3\b\3\b\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3"+
		"\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21\3\21\3\22\3\22\7\22\177\n\22"+
		"\f\22\16\22\u0082\13\22\3\23\5\23\u0085\n\23\3\23\5\23\u0088\n\23\3\23"+
		"\3\23\3\24\3\24\3\24\3\24\3\25\3\25\7\25\u0092\n\25\f\25\16\25\u0095\13"+
		"\25\3\26\3\26\3\26\3\26\5\26\u009b\n\26\3\27\5\27\u009e\n\27\3\27\5\27"+
		"\u00a1\n\27\3\27\3\27\3\30\3\30\3\30\3\30\3\31\3\31\3\31\7\31\u00ac\n"+
		"\31\f\31\16\31\u00af\13\31\3\32\3\32\5\32\u00b3\n\32\3\33\3\33\5\33\u00b7"+
		"\n\33\3\34\3\34\3\34\3\34\3\34\7\34\u00be\n\34\f\34\16\34\u00c1\13\34"+
		"\3\34\2\2\35\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,.\60\62\64"+
		"\66\2\3\3\2\b\t\2\u00ba\28\3\2\2\2\4N\3\2\2\2\6U\3\2\2\2\bW\3\2\2\2\n"+
		"`\3\2\2\2\fc\3\2\2\2\16e\3\2\2\2\20i\3\2\2\2\22k\3\2\2\2\24m\3\2\2\2\26"+
		"o\3\2\2\2\30q\3\2\2\2\32s\3\2\2\2\34u\3\2\2\2\36w\3\2\2\2 y\3\2\2\2\""+
		"|\3\2\2\2$\u0084\3\2\2\2&\u008b\3\2\2\2(\u008f\3\2\2\2*\u009a\3\2\2\2"+
		",\u009d\3\2\2\2.\u00a4\3\2\2\2\60\u00a8\3\2\2\2\62\u00b0\3\2\2\2\64\u00b4"+
		"\3\2\2\2\66\u00b8\3\2\2\28:\5\24\13\29;\5\26\f\2:9\3\2\2\2:;\3\2\2\2;"+
		"<\3\2\2\2<=\5\4\3\2=>\7\6\2\2>@\5\32\16\2?A\7\4\2\2@?\3\2\2\2@A\3\2\2"+
		"\2AC\3\2\2\2BD\5(\25\2CB\3\2\2\2CD\3\2\2\2DF\3\2\2\2EG\5\"\22\2FE\3\2"+
		"\2\2FG\3\2\2\2GI\3\2\2\2HJ\5\60\31\2IH\3\2\2\2IJ\3\2\2\2J\3\3\2\2\2KM"+
		"\5\n\6\2LK\3\2\2\2MP\3\2\2\2NL\3\2\2\2NO\3\2\2\2OQ\3\2\2\2PN\3\2\2\2Q"+
		"R\5\6\4\2R\5\3\2\2\2SV\5\16\b\2TV\5\20\t\2US\3\2\2\2UT\3\2\2\2V\7\3\2"+
		"\2\2WX\5\f\7\2XY\7\23\2\2Y\\\5\22\n\2Z[\7\23\2\2[]\5\22\n\2\\Z\3\2\2\2"+
		"]^\3\2\2\2^\\\3\2\2\2^_\3\2\2\2_\t\3\2\2\2`a\5\6\4\2ab\7\24\2\2b\13\3"+
		"\2\2\2cd\7\27\2\2d\r\3\2\2\2ef\5\f\7\2fg\7\23\2\2gh\5\22\n\2h\17\3\2\2"+
		"\2ij\5\f\7\2j\21\3\2\2\2kl\7\27\2\2l\23\3\2\2\2mn\7\3\2\2n\25\3\2\2\2"+
		"op\7\16\2\2p\27\3\2\2\2qr\t\2\2\2r\31\3\2\2\2st\5 \21\2t\33\3\2\2\2uv"+
		"\7\27\2\2v\35\3\2\2\2wx\7\27\2\2x\37\3\2\2\2yz\5\34\17\2z{\5\36\20\2{"+
		"!\3\2\2\2|\u0080\5$\23\2}\177\5$\23\2~}\3\2\2\2\177\u0082\3\2\2\2\u0080"+
		"~\3\2\2\2\u0080\u0081\3\2\2\2\u0081#\3\2\2\2\u0082\u0080\3\2\2\2\u0083"+
		"\u0085\5\30\r\2\u0084\u0083\3\2\2\2\u0084\u0085\3\2\2\2\u0085\u0087\3"+
		"\2\2\2\u0086\u0088\7\5\2\2\u0087\u0086\3\2\2\2\u0087\u0088\3\2\2\2\u0088"+
		"\u0089\3\2\2\2\u0089\u008a\5&\24\2\u008a%\3\2\2\2\u008b\u008c\5\6\4\2"+
		"\u008c\u008d\7\20\2\2\u008d\u008e\5*\26\2\u008e\'\3\2\2\2\u008f\u0093"+
		"\5,\27\2\u0090\u0092\5,\27\2\u0091\u0090\3\2\2\2\u0092\u0095\3\2\2\2\u0093"+
		"\u0091\3\2\2\2\u0093\u0094\3\2\2\2\u0094)\3\2\2\2\u0095\u0093\3\2\2\2"+
		"\u0096\u0097\7\25\2\2\u0097\u0098\7\27\2\2\u0098\u009b\7\25\2\2\u0099"+
		"\u009b\7\30\2\2\u009a\u0096\3\2\2\2\u009a\u0099\3\2\2\2\u009b+\3\2\2\2"+
		"\u009c\u009e\5\30\r\2\u009d\u009c\3\2\2\2\u009d\u009e\3\2\2\2\u009e\u00a0"+
		"\3\2\2\2\u009f\u00a1\7\5\2\2\u00a0\u009f\3\2\2\2\u00a0\u00a1\3\2\2\2\u00a1"+
		"\u00a2\3\2\2\2\u00a2\u00a3\5.\30\2\u00a3-\3\2\2\2\u00a4\u00a5\5\b\5\2"+
		"\u00a5\u00a6\7\20\2\2\u00a6\u00a7\5*\26\2\u00a7/\3\2\2\2\u00a8\u00a9\7"+
		"\n\2\2\u00a9\u00ad\5\62\32\2\u00aa\u00ac\5\62\32\2\u00ab\u00aa\3\2\2\2"+
		"\u00ac\u00af\3\2\2\2\u00ad\u00ab\3\2\2\2\u00ad\u00ae\3\2\2\2\u00ae\61"+
		"\3\2\2\2\u00af\u00ad\3\2\2\2\u00b0\u00b2\5\64\33\2\u00b1\u00b3\7\24\2"+
		"\2\u00b2\u00b1\3\2\2\2\u00b2\u00b3\3\2\2\2\u00b3\63\3\2\2\2\u00b4\u00b6"+
		"\5\66\34\2\u00b5\u00b7\7\13\2\2\u00b6\u00b5\3\2\2\2\u00b6\u00b7\3\2\2"+
		"\2\u00b7\65\3\2\2\2\u00b8\u00b9\5\f\7\2\u00b9\u00ba\7\23\2\2\u00ba\u00bf"+
		"\5\22\n\2\u00bb\u00bc\7\23\2\2\u00bc\u00be\5\22\n\2\u00bd\u00bb\3\2\2"+
		"\2\u00be\u00c1\3\2\2\2\u00bf\u00bd\3\2\2\2\u00bf\u00c0\3\2\2\2\u00c0\67"+
		"\3\2\2\2\u00c1\u00bf\3\2\2\2\25:@CFINU^\u0080\u0084\u0087\u0093\u009a"+
		"\u009d\u00a0\u00ad\u00b2\u00b6\u00bf";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}