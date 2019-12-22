package cz.cvut.kbss.jopa.query.soql;

public class SoqlAttribute extends SoqlParam {

    private String value;

    private boolean isNot = false;

    private String operator;

    private String prefix = "http://www.example.org/";

    private String rdfType = "rdf:type";

    public SoqlAttribute() {
        super();
    }

    public String getValue() { return value; }

    public void setValue(String value) { this.value = value; }

    public boolean isNot() { return isNot; }

    public void setNot(boolean not) { isNot = not; }

    public void setOperator(String operator) { this.operator = operator; }

    public String getOperator() { return operator; }

    public void setPrefix(String prefix){ this.prefix = prefix; }

    public String getPrefix(){ return this.prefix; }

    public void setRdfType(String rdfType){ this.rdfType = rdfType; }

    public String getRdfType(){ return this.rdfType; }

    public boolean isFilter() {
        return !operator.isEmpty() && !operator.equals("=");
    }

    private boolean isTable(){ return !getFirstNode().hasNextChild(); }

    private boolean isValueParam(){
        return !operator.isEmpty() && value.charAt(0) == ':';
    }

    public String getFilter(){
        StringBuilder buildFilter = new StringBuilder();
        if(operator.equals("LIKE")){
            buildFilter.append("regex(").append(getAsParam()).append(", ?").append(this.value.substring(1)).append(") ");
        } else {
            buildFilter.append(getAsParam()).append(" ").append(this.operator).append(" ").append("?").append(this.value.substring(1));
        }
        return buildFilter.toString();
    }

    public String getTripplePattern(){
        StringBuilder buildTP = new StringBuilder("?x ");
        if(isTable()){
            buildTP.append(getRdfType()).append(" ")
                    .append(toIri(getFirstNode().getValue())).append(" . ");
        }else{
            SoqlNode pointer = getFirstNode().getChild();
            StringBuilder buildParam = new StringBuilder("?");
            buildParam.append(getFirstNode().getValue());
            buildParam.append(pointer.getCapitalizedvalue());
            String param = "";
            if (pointer.hasNextChild()){
                param = "?" + pointer.getValue();
            }else{
                if (isFilter()){
                    param = buildParam.toString();
                }else{
                    param = "?" + this.value.substring(1);
                }
            }
            buildTP.append(toIri(pointer.getValue())).append(" ").append(param).append(" . ");
            while(pointer.hasNextChild()){
                SoqlNode newPointer = pointer.getChild();
                buildTP.append("?").append(pointer.getValue())
                        .append(" ").append(toIri(newPointer.getValue())).append(" ");
                buildParam.append(newPointer.getCapitalizedvalue());
                if(newPointer.hasNextChild()){
                    buildTP.append("?").append(pointer.getChild().getValue());
                }else{
                    if (isFilter()){
                        buildTP.append(buildParam);
                    }else{
                        buildTP.append("?").append(this.value.substring(1));
                    }
                }
                buildTP.append(" . ");
                pointer = newPointer;
            }
        }
        return buildTP.toString();
    }

    private StringBuilder toIri(String param){
        StringBuilder sb = new StringBuilder("<");
        sb.append(getPrefix()).append(param).append(">");
        return sb;
    }
}
