package cz.cvut.kbss.jopa.query.soql;


import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;

public class SoqlQueryListener implements soqlListener {

    private String newQuery;
    private String prefix = "http://www.example.org/";

    private String typeDef;

    private boolean orExists = false;

    // contains Couple of attributes (Owner, Attribute)
    private ArrayList<MyPair<String, String>> attributes;

    // contains prefix for every Column (p -> Person)
    private HashMap<String, String> prefixies;

    // contains Couple of attributes and filter condiitions: (Owner, Attribute) -> (Operator, Value/QueryParameter)
    private LinkedHashMap<MyPair<String, String>, MyPair<String, String>> filters;

    // contains Couple of attributes and filter condiitions: (Owner, Attribute) -> (Operator, Value/QueryParameter)
    private LinkedHashMap<MyPair<String, String>, MyPair<String, String>> filtersNot;

    // contains chain of couples of attributes.
    /* if NOT is not present and operator is '=' then last chain contains (Operator, Value/QueryParameter), else contains parameter,
    and QueryParameter is placed into filters/filtersNot by rule lastCouple -> (Operator, Value/QueryParameter) */
    // Example: p.phone.number = :phoneNumber: (p, phone) -> (phone, number), (phone, number) -> ( = , :phoneNumber)
    // Example: p.phone.mobile.number = :phoneNumber: (p, phone) -> (phone, number), (phone, mobile) -> (mobile, number), (mobile, number) -> ( = , :phoneNumber)
    private HashMap<MyPair<String, String>, MyPair<String, String>> joins;

    //contains Couple of attributes of filters and their logical operator: (Owner, Attribute) -> "AND"
    private HashMap<MyPair<String, String>, String> logicOp;



    public SoqlQueryListener() {
        super();
        this.newQuery = "";
        this.typeDef = "SELECT";
        this.attributes = new ArrayList<>();
        this.prefixies = new HashMap<>();
        this.filters = new LinkedHashMap<>();
        this.filtersNot = new LinkedHashMap<>();
        this.joins = new HashMap<>();
        this.logicOp = new HashMap<>();
    }

    @Override
    public void enterQuerySentence(soqlParser.QuerySentenceContext ctx) { }

    @Override
    public void exitQuerySentence(soqlParser.QuerySentenceContext ctx) {
        buildString();
    }

    @Override
    public void enterParams(soqlParser.ParamsContext ctx) {

    }

    @Override
    public void exitParams(soqlParser.ParamsContext ctx) {

    }

    @Override
    public void enterParam(soqlParser.ParamContext ctx) {}

    @Override
    public void exitParam(soqlParser.ParamContext ctx) {
    }

    @Override
    public void enterJoinedParams(soqlParser.JoinedParamsContext ctx) {

    }

    @Override
    public void exitJoinedParams(soqlParser.JoinedParamsContext ctx) {

    }

    @Override
    public void enterParamComma(soqlParser.ParamCommaContext ctx) {

    }

    @Override
    public void exitParamComma(soqlParser.ParamCommaContext ctx) {

    }

    @Override
    public void enterObject(soqlParser.ObjectContext ctx) {

    }

    @Override
    public void exitObject(soqlParser.ObjectContext ctx) {

    }

    @Override
    public void enterObjWithAttr(soqlParser.ObjWithAttrContext ctx) {
        String owner = getOwnerfromParam(ctx);
        String attribute = getAttributefromParam(ctx);
        attributes.add(MyPair.of(owner, attribute));
    }

    @Override
    public void exitObjWithAttr(soqlParser.ObjWithAttrContext ctx) {

    }

    @Override
    public void enterObjWithOutAttr(soqlParser.ObjWithOutAttrContext ctx) {
        String owner = getOwnerfromParam(ctx);
        attributes.add(MyPair.of(owner, ""));
    }

    @Override
    public void exitObjWithOutAttr(soqlParser.ObjWithOutAttrContext ctx) {

    }

    @Override
    public void enterAttribute(soqlParser.AttributeContext ctx) {

    }

    @Override
    public void exitAttribute(soqlParser.AttributeContext ctx) {

    }

    @Override
    public void enterTypeDef(soqlParser.TypeDefContext ctx) {
        typeDef = ctx.getChild(0).getText();
    }

    @Override
    public void exitTypeDef(soqlParser.TypeDefContext ctx) {}

    @Override
    public void enterLogOp(soqlParser.LogOpContext ctx) {
    }

    @Override
    public void exitLogOp(soqlParser.LogOpContext ctx) { }

    @Override
    public void enterTables(soqlParser.TablesContext ctx) {

    }

    @Override
    public void exitTables(soqlParser.TablesContext ctx) {

    }

    @Override
    public void enterTable(soqlParser.TableContext ctx) {

    }

    @Override
    public void exitTable(soqlParser.TableContext ctx) {

    }

    @Override
    public void enterTableName(soqlParser.TableNameContext ctx) {

    }

    @Override
    public void exitTableName(soqlParser.TableNameContext ctx) {

    }

    @Override
    public void enterTableWithName(soqlParser.TableWithNameContext ctx) {
        String table = ctx.getChild(0).getChild(0).getText();
        String tableName = ctx.getChild(1).getChild(0).getText();
        if(!prefixies.containsKey(tableName)){
            prefixies.put(tableName, table);
        }
    }

    @Override
    public void exitTableWithName(soqlParser.TableWithNameContext ctx) {

    }

    @Override
    public void enterWhereClausules(soqlParser.WhereClausulesContext ctx) { }

    @Override
    public void exitWhereClausules(soqlParser.WhereClausulesContext ctx) {
    }

    @Override
    public void enterWhereClausuleNot(soqlParser.WhereClausuleNotContext ctx) {

    }

    @Override
    public void exitWhereClausuleNot(soqlParser.WhereClausuleNotContext ctx) {

    }

    @Override
    public void enterWhereClausule(soqlParser.WhereClausuleContext ctx) {
        ParseTree objWithAttr = ctx.getChild(0).getChild(0);
        ParseTree value = ctx.getChild(2); // whereClausuleValue Node
        boolean isNot = hasNot(ctx.getParent());
        String logicalOperator = getLogicalOperator(ctx, 3);
        String tableName = objWithAttr.getChild(0).getChild(0).getText();
        String attributeName = objWithAttr.getChild(2).getChild(0).getText();
        MyPair<String, String> param = MyPair.of(tableName, attributeName);
        //pro odstraneni problemu s OR
        String operator = ctx.getChild(1).getText();
        String operand = value.getChildCount() > 1 ? value.getChild(1).getText() : value.getChild(0).getText();
        MyPair<String, String> filter = MyPair.of(operator, operand);
        if (isNot){
            filtersNot.put(param, filter);
        } else {
            filters.put(param, filter);
        }
        if(!logicalOperator.isEmpty()){
            logicOp.put(param, logicalOperator);
        }

        //PROBLEM S OR
//        if (isNot){
//            String operand = value.getChildCount() > 1 ? value.getChild(1).getText() : value.getChild(0).getText();
//            MyPair<String, String> filter = MyPair.of(operator, operand);
//            filtersNot.put(param, filter);
//            if(!logicalOperator.isEmpty()){
//                logicOp.put(param, logicalOperator);
//            }
//        } else {
//            if (!(operator.equals("=") && (logicalOperator.equals("AND") || logicalOperator.isEmpty()))) {
//                String operand = value.getChildCount() > 1 ? value.getChild(1).getText() : value.getChild(0).getText();
//                MyPair<String, String> filter = MyPair.of(operator, operand);
//                filters.put(param, filter);
//                if (!logicalOperator.isEmpty()) {
//                    logicOp.put(param, logicalOperator);
//                }
//            }
//        }
    }

    @Override
    public void exitWhereClausule(soqlParser.WhereClausuleContext ctx) {

    }

    @Override
    public void enterWhereClausuleJoin(soqlParser.WhereClausuleJoinContext ctx) {

    }

    @Override
    public void exitWhereClausuleJoin(soqlParser.WhereClausuleJoinContext ctx) {

    }

    @Override
    public void enterWhereClausuleValue(soqlParser.WhereClausuleValueContext ctx) {

    }

    @Override
    public void exitWhereClausuleValue(soqlParser.WhereClausuleValueContext ctx) {

    }

    @Override
    public void enterClausuleJoinNot(soqlParser.ClausuleJoinNotContext ctx) {

    }

    @Override
    public void exitClausuleJoinNot(soqlParser.ClausuleJoinNotContext ctx) {

    }

    @Override
    public void enterClausuleJoin(soqlParser.ClausuleJoinContext ctx) {
        boolean isNot = hasNot(ctx.getParent());
        ParseTree joinedParams = ctx.getChild(0);
        ParseTree operatorNode = ctx.getChild(1);
        ParseTree whereClausuleValue = ctx.getChild(2);
        String logicalOperator = getLogicalOperator(ctx, 3);

        String owner = getOwnerfromParam(joinedParams);
        String attribute = getAttributefromParam(joinedParams);
        MyPair<String, String> prevPair = MyPair.of(owner, attribute);
        attributes.add(prevPair);

        for(int i = 4; i < joinedParams.getChildCount(); i += 2){
            String associatedTo = joinedParams.getChild(i-2).getChild(0).getText();
            String associatedName = joinedParams.getChild(i).getChild(0).getText();
            MyPair<String, String> actualPair = MyPair.of(associatedTo, associatedName);
            joins.put(prevPair, actualPair);
            prevPair = actualPair;
        }
        String operator = operatorNode.getText();

        /* QueryParameter has to go to filters/filetrsNot, last couple for joins will be replaced by own parameter
        example: p.phone.number => pPhoneNumber */
        String ownParameter = owner + toUc(prevPair.getFirst()) + toUc(prevPair.getSecond());
        MyPair<String, String> finalPair = MyPair.of(operator, ownParameter);
        joins.put(prevPair, finalPair);
        MyPair<String, String> filterKey = MyPair.of(owner + toUc(prevPair.getFirst()), prevPair.getSecond());
        MyPair<String, String> filterPair = MyPair.of( operator, whereClausuleValue.getText());
        if(isNot){
            filtersNot.put(filterKey, filterPair);
        }else{
            filters.put(filterKey, filterPair);
        }
        if(!logicalOperator.isEmpty()){
            logicOp.put(filterKey, logicalOperator);
        }

        // STARA implementace - problem s OR
//        if (operator.equals("=") && !isNot && !logicalOperator.equals("OR")){
//            MyPair<String, String> finalPair = MyPair.of(operator, whereClausuleValue.getText());
//            joins.put(prevPair, finalPair);
//        }else{
//            /* QueryParameter has to go to filters/filetrsNot, last couple for joins will be replaced by own parameter
//            example: p.phone.number => pPhoneNumber */
//            String ownParameter = owner + toUc(prevPair.getFirst()) + toUc(prevPair.getSecond());
//            MyPair<String, String> finalPair = MyPair.of(operator, ownParameter);
//            joins.put(prevPair, finalPair);
//            MyPair<String, String> filterKey = MyPair.of(owner + toUc(prevPair.getFirst()), prevPair.getSecond());
//            MyPair<String, String> filterPair = MyPair.of( operator, whereClausuleValue.getText());
//            if(isNot){
//                filtersNot.put(filterKey, filterPair);
//            }else{
//                filters.put(filterKey, filterPair);
//            }
//            if(!logicalOperator.isEmpty()){
//                logicOp.put(filterKey, logicalOperator);
//            }
//        }
    }

    @Override
    public void exitClausuleJoin(soqlParser.ClausuleJoinContext ctx) {

    }

    @Override
    public void visitTerminal(TerminalNode terminalNode) {

    }

    @Override
    public void visitErrorNode(ErrorNode errorNode) {

    }

    @Override
    public void enterEveryRule(ParserRuleContext parserRuleContext) {

    }

    @Override
    public void exitEveryRule(ParserRuleContext parserRuleContext) {

    }

    //Methods to help parse tree
    private String getOwnerfromParam(ParserRuleContext ctx){
        return ctx.getChild(0).getChild(0).getText();
    }

    private String getOwnerfromParam(ParseTree child){
        return child.getChild(0).getChild(0).getText();
    }

    private String getAttributefromParam(ParserRuleContext ctx){
        return ctx.getChild(2).getChild(0).getText();
    }

    private String getAttributefromParam(ParseTree child){
        return child.getChild(2).getChild(0).getText();
    }

    private boolean hasNot(ParserRuleContext ctx){
        return ctx.getChildCount() > 1;
    }

    private String getLogicalOperator(ParserRuleContext ctx, int position){
        String operator = "" ;
        if (ctx.getChildCount() == (position + 1)){
            operator = ctx.getChild(position).getChild(0).getText();
        }
        return operator;
    }

    private String toUc(String s){
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    public String getSoqlQuery(){
        return newQuery;
    }

    //Methods to build new Query
    private void buildString(){
        if(attributes.isEmpty()){
            return;
        }
        StringBuilder newQueryBuilder = new StringBuilder(typeDef);
        newQueryBuilder.append(" ?x WHERE { ").append(processAttributes());
        if(!filters.isEmpty()){
            newQueryBuilder.append(processFilters());
        }
        if(!filtersNot.isEmpty()){
            newQueryBuilder.append(processFiltersNot());
        }
        newQueryBuilder.append("}");
        newQuery = newQueryBuilder.toString();
    }

    private StringBuilder processAttributes(){
        StringBuilder attributesPart = new StringBuilder();
        for (MyPair<String, String> attr: attributes) {
            StringBuilder partBuilder = new StringBuilder();
            if(attr.isSecondEqual("")) {
                partBuilder.append(buildObject(attr.getFirst()));
            }else{
                partBuilder.append(buildParam(attr));
            }
            attributesPart.append(partBuilder);
        }
        return attributesPart;
    }

    private StringBuilder processFilters(){
        StringBuilder filtersPart = new StringBuilder("FILTER (");
        filtersPart.append(processFilter(filters));
        return filtersPart;
    }

    private StringBuilder processFiltersNot(){
        StringBuilder filtersPart = new StringBuilder("FILTER NOT EXISTS(");
        filtersPart.append(processFilter(filtersNot));
        return filtersPart;
    }

    private StringBuilder processFilter(LinkedHashMap<MyPair<String, String>, MyPair<String, String>> filter){
        StringBuilder filtersPart = new StringBuilder();
        Iterator<MyPair<String, String>> keyIterator = filter.keySet().iterator();
        while(keyIterator.hasNext()){
            MyPair<String, String> key = keyIterator.next();
            MyPair<String, String> value = filter.get(key);
            filtersPart.append("?").append(key.toQueryParam()).append(" ").append(value.getFirst()).append(" ");
            if(key.getSecond().charAt(0) == ':'){
                filtersPart.append("?").append(key.getSecond());
            }else{
                filtersPart.append(value.getSecond());
            }
            if (logicOp.containsKey(key) && keyIterator.hasNext()){
                filtersPart.append(" ").append(parseLogOp(logicOp.get(key))).append(" ");
            }
        }
//        int filterSize = filter.size();
//        filter.forEach((MyPair<String, String> k, MyPair<String, String> v) -> {
//            filtersPart.append("?").append(k.toQueryParam()).append(" ").append(v.getFirst()).append(" ");
//            if(v.getSecond().charAt(0) == ':'){
//                filtersPart.append("?").append(k.getSecond());
//            }else{
//                filtersPart.append(v.getSecond());
//            }
//            if (logicOp.containsKey(k) && filterSize > 1){
//                filtersPart.append(" ").append(parseLogOp(logicOp.get(k))).append(" ");
//            }
//        });
        filtersPart.append(")");
        return filtersPart;
    }

    private StringBuilder buildObject(String a){
        StringBuilder sb = new StringBuilder("?x rdf:type ");
        sb.append(toIri(prefixies.get(a))).append(" . ");
        return sb;
    }

    private StringBuilder buildParam(MyPair<String, String> attr){
        StringBuilder sb = new StringBuilder("?x ");
        sb.append(toIri(attr.getSecond())).append(" ");
        if(filters.containsKey(attr) || filtersNot.containsKey(attr)){
            sb.append("?").append(attr.toQueryParam());
        }else{
            sb.append("?").append(attr.getSecond());
        }
        sb.append(" . ");
        if(joins.containsKey(attr)){
            sb.append(buildJoinedParams(joins.get(attr)));
        }
        return sb;
    }

    private StringBuilder buildJoinedParams(MyPair<String, String> attr){
        StringBuilder sb = new StringBuilder("?");
        MyPair<String, String> keyPair = attr;
        while(joins.containsKey(keyPair)){
            MyPair<String, String> newPair = joins.get(keyPair);

            sb.append(keyPair.getFirst()).append(" ").append(toIri(keyPair.getSecond())).append(" ?");
            String lastParam = newPair.getSecond();
            if(lastParam.charAt(0) == ':'){
                lastParam = lastParam.substring(1);
            }
            sb.append(lastParam).append(" . ");
            keyPair = newPair;
        }
        return sb;
    }

    private StringBuilder toIri(String param){
        StringBuilder sb = new StringBuilder("<");
        sb.append(prefix).append(param).append(">");
        return sb;
    }

    private String parseLogOp(String logOp){
        switch (logOp) {
            case "AND":
                return "&&";
            case "OR":
                return "||";
            default:
                return "";
        }
    }
}
