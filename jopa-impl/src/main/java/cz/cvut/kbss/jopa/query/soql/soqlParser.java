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
		SELECT=1, WHERE=2, NOT=3, FROM=4, JOIN=5, AND=6, OR=7, LEFTOUTERJOIN=8, 
		QUERYOPERATOR=9, RIGHTPAREN=10, LEFTPAREN=11, DOT=12, COMMA=13, QMARK=14, 
		COLON=15, TEXT=16, COLONTEXT=17, UPPERCASE=18, LOWERCASE=19, DIGIT=20, 
		NUMBER=21, VALUE=22, WHITESPACE=23;
	public static final int
		RULE_querySentence = 0, RULE_params = 1, RULE_param = 2, RULE_joinedParams = 3, 
		RULE_paramComma = 4, RULE_object = 5, RULE_objWithAttr = 6, RULE_objWithOutAttr = 7, 
		RULE_attribute = 8, RULE_typeDef = 9, RULE_logOp = 10, RULE_tables = 11, 
		RULE_table = 12, RULE_tableName = 13, RULE_tableWithName = 14, RULE_whereClausules = 15, 
		RULE_whereClausuleNot = 16, RULE_whereClausule = 17, RULE_whereClausuleJoin = 18, 
		RULE_whereClausuleValue = 19, RULE_clausuleJoinNot = 20, RULE_clausuleJoin = 21;
	public static final String[] ruleNames = {
		"querySentence", "params", "param", "joinedParams", "paramComma", "object", 
		"objWithAttr", "objWithOutAttr", "attribute", "typeDef", "logOp", "tables", 
		"table", "tableName", "tableWithName", "whereClausules", "whereClausuleNot", 
		"whereClausule", "whereClausuleJoin", "whereClausuleValue", "clausuleJoinNot", 
		"clausuleJoin"
	};

	private static final String[] _LITERAL_NAMES = {
		null, null, null, null, null, null, null, null, null, null, "')'", "'('", 
		"'.'", "','", "'\"'", "':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "LEFTOUTERJOIN", 
		"QUERYOPERATOR", "RIGHTPAREN", "LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", 
		"TEXT", "COLONTEXT", "UPPERCASE", "LOWERCASE", "DIGIT", "NUMBER", "VALUE", 
		"WHITESPACE"
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
		public TerminalNode WHERE() { return getToken(soqlParser.WHERE, 0); }
		public WhereClausuleJoinContext whereClausuleJoin() {
			return getRuleContext(WhereClausuleJoinContext.class,0);
		}
		public WhereClausulesContext whereClausules() {
			return getRuleContext(WhereClausulesContext.class,0);
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
			setState(44);
			typeDef();
			setState(45);
			params();
			setState(46);
			match(FROM);
			setState(47);
			tables();
			setState(49);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHERE) {
				{
				setState(48);
				match(WHERE);
				}
			}

			setState(52);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				{
				setState(51);
				whereClausuleJoin();
				}
				break;
			}
			setState(55);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				{
				setState(54);
				whereClausules();
				}
				break;
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
			setState(60);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(57);
					paramComma();
					}
					} 
				}
				setState(62);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			}
			setState(63);
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
			setState(67);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(65);
				objWithAttr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(66);
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
			setState(69);
			object();
			setState(70);
			match(DOT);
			setState(71);
			attribute();
			setState(74); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(72);
				match(DOT);
				setState(73);
				attribute();
				}
				}
				setState(76); 
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
			setState(78);
			param();
			setState(79);
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
			setState(81);
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
			setState(83);
			object();
			setState(84);
			match(DOT);
			setState(85);
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
			setState(87);
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
			setState(89);
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
			setState(91);
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
		enterRule(_localctx, 20, RULE_logOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(93);
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
		enterRule(_localctx, 22, RULE_tables);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(95);
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
		enterRule(_localctx, 24, RULE_table);
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
		enterRule(_localctx, 26, RULE_tableName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(99);
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
		enterRule(_localctx, 28, RULE_tableWithName);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(101);
			table();
			setState(102);
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
		enterRule(_localctx, 30, RULE_whereClausules);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(107);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==NOT || _la==TEXT) {
				{
				{
				setState(104);
				whereClausuleNot();
				}
				}
				setState(109);
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
		enterRule(_localctx, 32, RULE_whereClausuleNot);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(111);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NOT) {
				{
				setState(110);
				match(NOT);
				}
			}

			setState(113);
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
		public LogOpContext logOp() {
			return getRuleContext(LogOpContext.class,0);
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
		enterRule(_localctx, 34, RULE_whereClausule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(115);
			param();
			setState(116);
			match(QUERYOPERATOR);
			setState(117);
			whereClausuleValue();
			setState(119);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==AND || _la==OR) {
				{
				setState(118);
				logOp();
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
		enterRule(_localctx, 36, RULE_whereClausuleJoin);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(124);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(121);
					clausuleJoinNot();
					}
					} 
				}
				setState(126);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
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
		enterRule(_localctx, 38, RULE_whereClausuleValue);
		try {
			setState(131);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case QMARK:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(127);
				match(QMARK);
				setState(128);
				match(TEXT);
				setState(129);
				match(QMARK);
				}
				}
				break;
			case COLONTEXT:
				enterOuterAlt(_localctx, 2);
				{
				setState(130);
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
		enterRule(_localctx, 40, RULE_clausuleJoinNot);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(134);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NOT) {
				{
				setState(133);
				match(NOT);
				}
			}

			setState(136);
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
		public LogOpContext logOp() {
			return getRuleContext(LogOpContext.class,0);
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
		enterRule(_localctx, 42, RULE_clausuleJoin);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(138);
			joinedParams();
			setState(139);
			match(QUERYOPERATOR);
			setState(140);
			whereClausuleValue();
			setState(142);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==AND || _la==OR) {
				{
				setState(141);
				logOp();
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

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\31\u0093\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\3\2\3\2\3\2\3\2\3\2"+
		"\5\2\64\n\2\3\2\5\2\67\n\2\3\2\5\2:\n\2\3\3\7\3=\n\3\f\3\16\3@\13\3\3"+
		"\3\3\3\3\4\3\4\5\4F\n\4\3\5\3\5\3\5\3\5\3\5\6\5M\n\5\r\5\16\5N\3\6\3\6"+
		"\3\6\3\7\3\7\3\b\3\b\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r"+
		"\3\16\3\16\3\17\3\17\3\20\3\20\3\20\3\21\7\21l\n\21\f\21\16\21o\13\21"+
		"\3\22\5\22r\n\22\3\22\3\22\3\23\3\23\3\23\3\23\5\23z\n\23\3\24\7\24}\n"+
		"\24\f\24\16\24\u0080\13\24\3\25\3\25\3\25\3\25\5\25\u0086\n\25\3\26\5"+
		"\26\u0089\n\26\3\26\3\26\3\27\3\27\3\27\3\27\5\27\u0091\n\27\3\27\2\2"+
		"\30\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,\2\3\3\2\b\t\2\u0089"+
		"\2.\3\2\2\2\4>\3\2\2\2\6E\3\2\2\2\bG\3\2\2\2\nP\3\2\2\2\fS\3\2\2\2\16"+
		"U\3\2\2\2\20Y\3\2\2\2\22[\3\2\2\2\24]\3\2\2\2\26_\3\2\2\2\30a\3\2\2\2"+
		"\32c\3\2\2\2\34e\3\2\2\2\36g\3\2\2\2 m\3\2\2\2\"q\3\2\2\2$u\3\2\2\2&~"+
		"\3\2\2\2(\u0085\3\2\2\2*\u0088\3\2\2\2,\u008c\3\2\2\2./\5\24\13\2/\60"+
		"\5\4\3\2\60\61\7\6\2\2\61\63\5\30\r\2\62\64\7\4\2\2\63\62\3\2\2\2\63\64"+
		"\3\2\2\2\64\66\3\2\2\2\65\67\5&\24\2\66\65\3\2\2\2\66\67\3\2\2\2\679\3"+
		"\2\2\28:\5 \21\298\3\2\2\29:\3\2\2\2:\3\3\2\2\2;=\5\n\6\2<;\3\2\2\2=@"+
		"\3\2\2\2><\3\2\2\2>?\3\2\2\2?A\3\2\2\2@>\3\2\2\2AB\5\6\4\2B\5\3\2\2\2"+
		"CF\5\16\b\2DF\5\20\t\2EC\3\2\2\2ED\3\2\2\2F\7\3\2\2\2GH\5\f\7\2HI\7\16"+
		"\2\2IL\5\22\n\2JK\7\16\2\2KM\5\22\n\2LJ\3\2\2\2MN\3\2\2\2NL\3\2\2\2NO"+
		"\3\2\2\2O\t\3\2\2\2PQ\5\6\4\2QR\7\17\2\2R\13\3\2\2\2ST\7\22\2\2T\r\3\2"+
		"\2\2UV\5\f\7\2VW\7\16\2\2WX\5\22\n\2X\17\3\2\2\2YZ\5\f\7\2Z\21\3\2\2\2"+
		"[\\\7\22\2\2\\\23\3\2\2\2]^\7\3\2\2^\25\3\2\2\2_`\t\2\2\2`\27\3\2\2\2"+
		"ab\5\36\20\2b\31\3\2\2\2cd\7\22\2\2d\33\3\2\2\2ef\7\22\2\2f\35\3\2\2\2"+
		"gh\5\32\16\2hi\5\34\17\2i\37\3\2\2\2jl\5\"\22\2kj\3\2\2\2lo\3\2\2\2mk"+
		"\3\2\2\2mn\3\2\2\2n!\3\2\2\2om\3\2\2\2pr\7\5\2\2qp\3\2\2\2qr\3\2\2\2r"+
		"s\3\2\2\2st\5$\23\2t#\3\2\2\2uv\5\6\4\2vw\7\13\2\2wy\5(\25\2xz\5\26\f"+
		"\2yx\3\2\2\2yz\3\2\2\2z%\3\2\2\2{}\5*\26\2|{\3\2\2\2}\u0080\3\2\2\2~|"+
		"\3\2\2\2~\177\3\2\2\2\177\'\3\2\2\2\u0080~\3\2\2\2\u0081\u0082\7\20\2"+
		"\2\u0082\u0083\7\22\2\2\u0083\u0086\7\20\2\2\u0084\u0086\7\23\2\2\u0085"+
		"\u0081\3\2\2\2\u0085\u0084\3\2\2\2\u0086)\3\2\2\2\u0087\u0089\7\5\2\2"+
		"\u0088\u0087\3\2\2\2\u0088\u0089\3\2\2\2\u0089\u008a\3\2\2\2\u008a\u008b"+
		"\5,\27\2\u008b+\3\2\2\2\u008c\u008d\5\b\5\2\u008d\u008e\7\13\2\2\u008e"+
		"\u0090\5(\25\2\u008f\u0091\5\26\f\2\u0090\u008f\3\2\2\2\u0090\u0091\3"+
		"\2\2\2\u0091-\3\2\2\2\17\63\669>ENmqy~\u0085\u0088\u0090";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}