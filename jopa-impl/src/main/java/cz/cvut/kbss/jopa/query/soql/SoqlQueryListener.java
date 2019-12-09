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

    // contains Couple of attributes (Owner, Attribute)
    private ArrayList<MyPair<String, String>> attributes;

    // contains prefix for every Column (p -> Person)
    private HashMap<String, String> prefixies;

    // contains Couple of attributes and filter condiitions: (Owner, Attribute) -> (Operator, Value/QueryParameter)
    private HashMap<MyPair<String, String>, MyPair<String, String>> filters;

    // contains Couple of attributes and filter condiitions: (Owner, Attribute) -> (Operator, Value/QueryParameter)
    private HashMap<MyPair<String, String>, MyPair<String, String>> filtersNot;

    // contains chain of couples of attributes.
    /* if NOT is not present and operator is '=' then last chain contains (Operator, Value/QueryParameter), else contains parameter,
    and QueryParameter is placed into filters/filtersNot by rule lastCouple -> (Operator, Value/QueryParameter) */
    // Example: p.phone.number = :phoneNumber: (p, phone) -> (phone, number), (phone, number) -> ( = , :phoneNumber)
    // Example: p.phone.mobile.number = :phoneNumber: (p, phone) -> (phone, number), (phone, mobile) -> (mobile, number), (mobile, number) -> ( = , :phoneNumber)
    private HashMap<MyPair<String, String>, MyPair<String, String>> joins;

    private HashMap<MyPair<String, String>, MyPair<String, String>> joinedNot;

    //contains Couple of attributes which start new OR bracket
    private ArrayList<MyPair<String, String>> opOr;

    private HashMap<MyPair<String, String>, String> filtersRegex;

    private LinkedHashMap<String, String> orderBy;



    public SoqlQueryListener() {
        super();
        this.newQuery = "";
        this.typeDef = "SELECT";
        this.attributes = new ArrayList<>();
        this.prefixies = new HashMap<>();
        this.filters = new HashMap<>();
        this.filtersNot = new HashMap<>();
        this.joins = new HashMap<>();
        this.joinedNot = new HashMap<>();
        this.filtersRegex = new HashMap<>();
        this.opOr = new ArrayList<>();
        this.orderBy = new LinkedHashMap<>();
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
        String[] operators = getOperators(ctx.getParent());
        String logicalOperator = operators[0];
        boolean hasNot = operators[1].equals("NOT");

        ParseTree objWithAttr = ctx.getChild(0).getChild(0);
        String operator = ctx.getChild(1).getText();
        // whereClausuleValue Node
        ParseTree value = ctx.getChild(2);

        String owner = getOwnerfromParam(objWithAttr);
        String attribute = getAttributefromParam(objWithAttr);
        MyPair<String, String> param = MyPair.of(owner, attribute);

        //v operandu se muze nachazet parameter :age, alebo hodnota "32"
        String operand = value.getChildCount() > 1 ? value.getChild(1).getText() : value.getChild(0).getText();
        MyPair<String, String> filter = MyPair.of(operator, operand);
        if (hasNot){
            filtersNot.put(param, filter);
        } else {
            if (!operator.equals("=")){
                filters.put(param, filter);
            }
        }
        if (logicalOperator.equals("OR")){
            opOr.add(param);
        }
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
        String[] operators = getOperators(ctx.getParent());
        String logicalOperator = operators[0];
        boolean hasNot = operators[1].equals("NOT");

        ParseTree joinedParams = ctx.getChild(0);
        ParseTree operatorNode = ctx.getChild(1);
        ParseTree whereClausuleValue = ctx.getChild(2);

        String owner = getOwnerfromParam(joinedParams);
        String attribute = getAttributefromParam(joinedParams);
        MyPair<String, String> firstPair = MyPair.of(owner, attribute);
        MyPair<String, String> prevPair = firstPair;
        attributes.add(prevPair);

        for(int i = 4; i < joinedParams.getChildCount(); i += 2){
            String associatedTo = joinedParams.getChild(i-2).getChild(0).getText();
            String associatedName = joinedParams.getChild(i).getChild(0).getText();
            MyPair<String, String> actualPair = MyPair.of(associatedTo, associatedName);
            joins.put(prevPair, actualPair);
            prevPair = actualPair;
        }
        String operator = operatorNode.getText();
        String ownParameter;
        if (operator.equals("=")){
            ownParameter = prevPair.toQueryParam();
        }else{
          /* QueryParameter has to go to filters/filetrsNot, last couple for joins will be replaced by own parameter
            example: p.phone.number => pPhoneNumber */
            ownParameter = owner + toUc(prevPair.getFirst()) + toUc(prevPair.getSecond());
            MyPair<String, String> filterKey = MyPair.of(owner + toUc(prevPair.getFirst()), prevPair.getSecond());
            MyPair<String, String> filterPair = MyPair.of( operator, whereClausuleValue.getText());
            filters.put(filterKey, filterPair);
        }
        MyPair<String, String> finalPair = MyPair.of(operator, ownParameter);
        if(hasNot){
            filtersNot.put(firstPair,MyPair.of(operator, whereClausuleValue.getText()));
            joinedNot.put(firstPair, finalPair);
        }
        joins.put(prevPair, finalPair);
        if (logicalOperator.equals("OR")){
            opOr.add(firstPair);
        }
    }

    @Override
    public void exitClausuleJoin(soqlParser.ClausuleJoinContext ctx) {

    }

    @Override
    public void enterOrderByClausule(soqlParser.OrderByClausuleContext ctx) {

    }

    @Override
    public void exitOrderByClausule(soqlParser.OrderByClausuleContext ctx) {

    }

    @Override
    public void enterOrderBySingleComma(soqlParser.OrderBySingleCommaContext ctx) {

    }

    @Override
    public void exitOrderBySingleComma(soqlParser.OrderBySingleCommaContext ctx) {

    }

    @Override
    public void enterOrderBySingle(soqlParser.OrderBySingleContext ctx) {
        ParseTree orderByParam = ctx.getChild(0);
        String orderingBy = "ASC";
        if (ctx.getChildCount() > 1){
            orderingBy = ctx.getChild(1).getText();
        }
        int paramsCount = orderByParam.getChildCount();
        boolean filtered = false;
        String owner = getOwnerfromParam(orderByParam);
        String attributeNonFilter;
        String attributeFilter;
        if(paramsCount > 3) {
            String owningAttribute = orderByParam.getChild(paramsCount-3).getChild(0).getText();
            String attribute = orderByParam.getChild(paramsCount-1).getChild(0).getText();
            MyPair<String, String> filterKey = MyPair.of(owner + toUc(owningAttribute), attribute);
            if(filters.containsKey(filterKey)){
                filtered = true;
            }
            if(joinedNot.containsKey(filterKey)){
                MyPair<String, String> valuePair = joinedNot.get(filterKey);
                if(!valuePair.getFirst().equals("=")){
                    filtered = true;
                }
            }
            attributeNonFilter = owningAttribute + toUc(attribute);
            attributeFilter = filterKey.toQueryParam();
        } else {
            String attribute = getAttributefromParam(orderByParam);
            MyPair<String, String> pair = MyPair.of(owner, attribute);
            if(filters.containsKey(pair)){
                filtered = true;
            }
            if(filtersNot.containsKey(pair)){
                MyPair<String, String> valuePair = filtersNot.get(pair);
                if(!valuePair.getFirst().equals("=")){
                    filtered = true;
                }
            }
            attributeNonFilter = attribute;
            attributeFilter = pair.toQueryParam();
        }
        if(filtered){
            orderBy.put(attributeFilter, orderingBy);
        }else{
            orderBy.put(attributeNonFilter, orderingBy);
        }
    }

    @Override
    public void exitOrderBySingle(soqlParser.OrderBySingleContext ctx) {

    }

    @Override
    public void enterOrderByParam(soqlParser.OrderByParamContext ctx) {

    }

    @Override
    public void exitOrderByParam(soqlParser.OrderByParamContext ctx) {

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

    private String[] getOperators(ParserRuleContext ctx){
        String[] operators;
        switch (ctx.getChildCount()){
            case 1:
                operators = new String[]{"", ""};
                break;
            case 2:
                if(ctx.getChild(0).getChildCount() > 0){
                    operators = new String[]{ctx.getChild(0).getChild(0).getText(), ""};
                }else{
                    operators = new String[]{"", "NOT"};
                }
                break;
            case 3:
                String logOp = ctx.getChild(0).getChild(0).getText();
                String invOp = ctx.getChild(1).getText();
                operators = new String[]{logOp, invOp};
                break;
            default:
                operators = new String[]{"", ""};
        }
        return operators;
    }

    private String toUc(String s){
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    public String getNewQuery(){
        return newQuery;
    }


    //Methods to build new Query
    private void buildString(){
        if(attributes.isEmpty()){
            return;
        }
        StringBuilder newQueryBuilder = new StringBuilder(typeDef);
        newQueryBuilder.append(" ?x WHERE { ");
        if(!opOr.isEmpty()){
            newQueryBuilder.append("{ ");
        }
        newQueryBuilder.append(processAttributes());
        if(!opOr.isEmpty()){
            newQueryBuilder.append(" } ");
        }
        newQueryBuilder.append("}");
        if(!orderBy.isEmpty()){
            newQueryBuilder.append(" ").append(buildOrdering());
        }
        newQuery = newQueryBuilder.toString();
    }

    private StringBuilder processAttributes(){
        StringBuilder attributesPart = new StringBuilder();
        ArrayList<MyPair<String,String>> toFilter = new ArrayList<>();
        ArrayList<MyPair<String,String>> toInvFilter = new ArrayList<>();
        for (MyPair<String, String> attr: attributes) {
            if(opOr.contains(attr)){
                StringBuilder orPart = new StringBuilder();
                orPart.append(processAllFilters(toFilter,toInvFilter));
                orPart.append(" } UNION { ");
                toFilter.clear();
                toInvFilter.clear();
                attributesPart.append(orPart);
            }
            if(filtersNot.containsKey(attr)){
                toInvFilter.add(attr);
            }else{
                if (filters.containsKey(attr)){
                    toFilter.add(attr);
                }
                attributesPart.append(processAttribute(attr));
            }
        }
        attributesPart.append(processAllFilters(toFilter,toInvFilter));
        return attributesPart;
    }

    private StringBuilder processAllFilters(ArrayList<MyPair<String,String>> toFilter, ArrayList<MyPair<String,String>> toInvFilter){
        StringBuilder part = new StringBuilder();
        if(!toFilter.isEmpty()){
            part.append(processFilter(toFilter, filters));
        }
        if(!toInvFilter.isEmpty()){
            part.append(processInvFilter(toInvFilter));
        }
        return part;
    }

    private StringBuilder processFilter(ArrayList<MyPair<String, String>> toFilter,
                                        HashMap<MyPair<String, String>, MyPair<String, String>> localFilters) {
        StringBuilder buildFilter = new StringBuilder();
        if (toFilter.isEmpty()) {
            return buildFilter;
        }
        buildFilter.append("FILTER (");
        for (MyPair<String, String> attr : toFilter) {
            if (toFilter.indexOf(attr) != 0) {
                buildFilter.append(" && ");
            }
            if(joinedNot.containsKey(attr)){
                MyPair<String, String> keyPair = joinedNot.get(attr);
                MyPair<String, String> valuePair = localFilters.get(attr);
                buildFilter.append("?").append(keyPair.getSecond()).append(" ").append(valuePair.getFirst()).append(" ");
                if (valuePair.getSecond().charAt(0) == ':') {
                    buildFilter.append("?").append(attr.getSecond().substring(1));
                } else {
                    buildFilter.append(valuePair.getSecond());
                }
            }else{
                MyPair<String, String> valuePair = localFilters.get(attr);
                buildFilter.append("?").append(attr.toQueryParam()).append(" ").append(valuePair.getFirst()).append(" ");
                buildFilter.append(valuePair.getSecond());
            }
        }
        buildFilter.append(") ");
        return buildFilter;
    }

    private StringBuilder processInvFilter(ArrayList<MyPair<String,String>> toInvFilter) {
        StringBuilder buildInvFilter = new StringBuilder();
        ArrayList<MyPair<String,String>> toFilter = new ArrayList<>();
        if (toInvFilter.isEmpty()) {
            return buildInvFilter;
        }
        buildInvFilter.append("FILTER NOT EXISTS ( ");
        for (MyPair<String, String> attr : toInvFilter) {
            buildInvFilter.append(processAttribute(attr));
            MyPair<String, String> pair = filtersNot.get(attr);
            if(!pair.getFirst().equals("=")){
                toFilter.add(attr);
            }
        }
        buildInvFilter.append(processFilter(toFilter, filtersNot)).append(")");
        return buildInvFilter;
    }

    private StringBuilder processAttribute(MyPair<String, String> attr) {
        StringBuilder buildAttr = new StringBuilder();
        if(attr.isSecondEqual("")) {
            buildAttr.append(buildObject(attr.getFirst()));
        }else{
            buildAttr.append(buildParam(attr));
        }
        return buildAttr;
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
            if(filtersNot.containsKey(attr)){
                MyPair<String, String> pair = filtersNot.get(attr);
                if(pair.getFirst().equals("=")){
                    sb.append("?").append(attr.getSecond());
                }else{
                    sb.append("?").append(attr.toQueryParam());
                }
            }else{
                sb.append("?").append(attr.toQueryParam());
            }
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
            sb.append(keyPair.getFirst()).append(" ").append(toIri(keyPair.getSecond())).append(" ?");
            MyPair<String, String> newPair = joins.get(keyPair);
            String lastParam = newPair.getSecond();
            if(lastParam.charAt(0) == ':'){
                lastParam = lastParam.substring(1);
            }
            sb.append(lastParam).append(" . ");
            keyPair = newPair;
        }
        return sb;
    }

    private StringBuilder buildOrdering(){
        StringBuilder sb = new StringBuilder("ORDER BY ");
        Iterator<String> iterator = orderBy.keySet().iterator();
        while(iterator.hasNext()){
            String key = iterator.next();
            String orderingBy = orderBy.get(key);
            if(orderingBy.equals("DESC")){
                sb.append("DESC(?").append(key).append(") ");
            } else {
                sb.append("?").append(key).append(" ");
            }
        }
        return sb;
    }

    private StringBuilder toIri(String param){
        StringBuilder sb = new StringBuilder("<");
        sb.append(prefix).append(param).append(">");
        return sb;
    }
}
