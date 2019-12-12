package cz.cvut.kbss.jopa.query.soql;// Generated from soql.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class soqlLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		SELECT=1, WHERE=2, NOT=3, FROM=4, JOIN=5, AND=6, OR=7, ORDERBY=8, ORDERING=9, 
		GROUPBY=10, ASC=11, DESC=12, DISTINCT=13, QUERYOPERATOR=14, DOT=15, COMMA=16, 
		QMARK=17, COLON=18, TEXT=19, COLONTEXT=20, UPPERCASE=21, LOWERCASE=22, 
		DIGIT=23, NUMBER=24, VALUE=25, WHITESPACE=26;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", "ORDERING", 
		"GROUPBY", "ASC", "DESC", "DISTINCT", "QUERYOPERATOR", "DOT", "COMMA", 
		"QMARK", "COLON", "TEXT", "COLONTEXT", "UPPERCASE", "LOWERCASE", "DIGIT", 
		"NUMBER", "VALUE", "WHITESPACE"
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


	public soqlLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "soql.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\34\u00b3\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\7\3"+
		"\7\3\7\3\7\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\5\n"+
		"e\n\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\r"+
		"\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\17\3\17"+
		"\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\5\17\u008c\n\17\3\20\3\20\3\21"+
		"\3\21\3\22\3\22\3\23\3\23\3\24\3\24\3\24\6\24\u0099\n\24\r\24\16\24\u009a"+
		"\3\25\3\25\3\25\3\26\3\26\3\27\3\27\3\30\3\30\3\31\6\31\u00a7\n\31\r\31"+
		"\16\31\u00a8\3\32\3\32\3\33\6\33\u00ae\n\33\r\33\16\33\u00af\3\33\3\33"+
		"\2\2\34\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17"+
		"\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\3\2\3"+
		"\4\2>>@@\2\u00bc\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13"+
		"\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2"+
		"\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2"+
		"!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3"+
		"\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\3\67\3\2\2\2"+
		"\5>\3\2\2\2\7D\3\2\2\2\tH\3\2\2\2\13M\3\2\2\2\rR\3\2\2\2\17V\3\2\2\2\21"+
		"Y\3\2\2\2\23d\3\2\2\2\25f\3\2\2\2\27o\3\2\2\2\31s\3\2\2\2\33x\3\2\2\2"+
		"\35\u008b\3\2\2\2\37\u008d\3\2\2\2!\u008f\3\2\2\2#\u0091\3\2\2\2%\u0093"+
		"\3\2\2\2\'\u0098\3\2\2\2)\u009c\3\2\2\2+\u009f\3\2\2\2-\u00a1\3\2\2\2"+
		"/\u00a3\3\2\2\2\61\u00a6\3\2\2\2\63\u00aa\3\2\2\2\65\u00ad\3\2\2\2\67"+
		"8\7U\2\289\7G\2\29:\7N\2\2:;\7G\2\2;<\7E\2\2<=\7V\2\2=\4\3\2\2\2>?\7Y"+
		"\2\2?@\7J\2\2@A\7G\2\2AB\7T\2\2BC\7G\2\2C\6\3\2\2\2DE\7P\2\2EF\7Q\2\2"+
		"FG\7V\2\2G\b\3\2\2\2HI\7H\2\2IJ\7T\2\2JK\7Q\2\2KL\7O\2\2L\n\3\2\2\2MN"+
		"\7L\2\2NO\7Q\2\2OP\7K\2\2PQ\7P\2\2Q\f\3\2\2\2RS\7C\2\2ST\7P\2\2TU\7F\2"+
		"\2U\16\3\2\2\2VW\7Q\2\2WX\7T\2\2X\20\3\2\2\2YZ\7Q\2\2Z[\7T\2\2[\\\7F\2"+
		"\2\\]\7G\2\2]^\7T\2\2^_\7\"\2\2_`\7D\2\2`a\7[\2\2a\22\3\2\2\2be\5\27\f"+
		"\2ce\5\31\r\2db\3\2\2\2dc\3\2\2\2e\24\3\2\2\2fg\7I\2\2gh\7T\2\2hi\7Q\2"+
		"\2ij\7W\2\2jk\7R\2\2kl\7\"\2\2lm\7D\2\2mn\7[\2\2n\26\3\2\2\2op\7C\2\2"+
		"pq\7U\2\2qr\7E\2\2r\30\3\2\2\2st\7F\2\2tu\7G\2\2uv\7U\2\2vw\7E\2\2w\32"+
		"\3\2\2\2xy\7F\2\2yz\7K\2\2z{\7U\2\2{|\7V\2\2|}\7K\2\2}~\7P\2\2~\177\7"+
		"E\2\2\177\u0080\7V\2\2\u0080\34\3\2\2\2\u0081\u008c\t\2\2\2\u0082\u0083"+
		"\7@\2\2\u0083\u008c\7?\2\2\u0084\u0085\7>\2\2\u0085\u008c\7?\2\2\u0086"+
		"\u008c\7?\2\2\u0087\u0088\7N\2\2\u0088\u0089\7K\2\2\u0089\u008a\7M\2\2"+
		"\u008a\u008c\7G\2\2\u008b\u0081\3\2\2\2\u008b\u0082\3\2\2\2\u008b\u0084"+
		"\3\2\2\2\u008b\u0086\3\2\2\2\u008b\u0087\3\2\2\2\u008c\36\3\2\2\2\u008d"+
		"\u008e\7\60\2\2\u008e \3\2\2\2\u008f\u0090\7.\2\2\u0090\"\3\2\2\2\u0091"+
		"\u0092\7$\2\2\u0092$\3\2\2\2\u0093\u0094\7<\2\2\u0094&\3\2\2\2\u0095\u0099"+
		"\5-\27\2\u0096\u0099\5+\26\2\u0097\u0099\5/\30\2\u0098\u0095\3\2\2\2\u0098"+
		"\u0096\3\2\2\2\u0098\u0097\3\2\2\2\u0099\u009a\3\2\2\2\u009a\u0098\3\2"+
		"\2\2\u009a\u009b\3\2\2\2\u009b(\3\2\2\2\u009c\u009d\5%\23\2\u009d\u009e"+
		"\5\'\24\2\u009e*\3\2\2\2\u009f\u00a0\4C\\\2\u00a0,\3\2\2\2\u00a1\u00a2"+
		"\4c|\2\u00a2.\3\2\2\2\u00a3\u00a4\4\62;\2\u00a4\60\3\2\2\2\u00a5\u00a7"+
		"\5/\30\2\u00a6\u00a5\3\2\2\2\u00a7\u00a8\3\2\2\2\u00a8\u00a6\3\2\2\2\u00a8"+
		"\u00a9\3\2\2\2\u00a9\62\3\2\2\2\u00aa\u00ab\5\61\31\2\u00ab\64\3\2\2\2"+
		"\u00ac\u00ae\7\"\2\2\u00ad\u00ac\3\2\2\2\u00ae\u00af\3\2\2\2\u00af\u00ad"+
		"\3\2\2\2\u00af\u00b0\3\2\2\2\u00b0\u00b1\3\2\2\2\u00b1\u00b2\b\33\2\2"+
		"\u00b2\66\3\2\2\2\t\2d\u008b\u0098\u009a\u00a8\u00af\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}