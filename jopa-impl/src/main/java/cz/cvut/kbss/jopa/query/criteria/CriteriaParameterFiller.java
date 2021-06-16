package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLiteralImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ParameterExpressionImpl;
import java.util.HashMap;

public class CriteriaParameterFiller {
    private HashMap<String, ExpressionLiteralImpl> literalParameters;
    private int counter;

    public CriteriaParameterFiller() {
        this.literalParameters = new HashMap<>();
        this.counter = 0;
    }

    /**
     * Register literal expression as query parameter and return generated name for query.
     * @param parameter - literal expression
     * @return String - generated name for query
     */
    public String registerParameter(ExpressionLiteralImpl parameter){
        String name = generateParameterName();
        literalParameters.put(name, parameter);
        return ":" + name;
    }

    /**
     * Register parameter expression as query parameter. Return real name if exists, generated name otherwise.
     * @param parameter - parameter expression
     * @return String - real name for query if exists, generated name for query otherwise
     */
    public String registerParameter(ParameterExpression parameter){
        if (parameter.getName() == null){
            String name = generateParameterName();
            ((ParameterExpressionImpl)parameter).setNameIfUnnamed(name);
        }
        return ":" + parameter.getName();
    }

    /**
     * Sets value from literal expressions registreted as parameters to query parameters.
     * @param query - TypedQuery fom setting parameters value
     */
    public <T> void setValuesToRegisteredParameters(TypedQueryImpl<T> query) {
        for(String name: literalParameters.keySet()){
            ExpressionLiteralImpl<?> parameter = literalParameters.get(name);
            if(parameter.getLanguageTag() != null){
                query.setParameter(name, (String) literalParameters.get(name).getValue(), parameter.getLanguageTag());
            } else {
                query.setParameter(name, literalParameters.get(name).getValue());
            }
        }
    }

    private String generateParameterName(){
        return "generatedName"+ this.counter++;
    }
}
