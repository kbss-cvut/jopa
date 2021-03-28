package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLiteralImpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

//TODO - BAKALARKA - doimplementovat registraciu parametrov
public class CriteriaParameterFiller {
    private HashMap<String, ExpressionLiteralImpl> parameters;

    public CriteriaParameterFiller() {
        this.parameters = new HashMap<>();
    }

    /**
     * Register literal expression as query parameter and return generated name for query.
     * @param parameter - literal expression
     * @return String - generated name for query
     */
    public String registerParameter(ExpressionLiteralImpl parameter){
        String name = generateParameterName();
        parameters.put(name, parameter);
        return ":" + name;
    }

    //TODO - BAKALARKA - konzultacia - VYRIESENE
    //    ParameterExpression<String> stringParameter = cb.parameter(String.class);
    //    Predicate predicate = cb.equal(root.get("username"), stringParameter);
    //                              "WHERE person.name = :parameter1"
    //    tq.setParameter(stringParameter,"Milan");
    //    ako TypedQuery spracuje nepomenovaný parameter? aký nazov parametru mám nastaviť?
    /**
     * Register parameter expression as query parameter. Return real name if exists, generated name otherwise.
     * @param parameter - parameter expression
     * @return String - real name for query if exists, generated name for query otherwise
     */
    public String registerParameter(ParameterExpression parameter){
        return "";
    }

    /**
     * Sets value from literal expressions registreted as parameters to query parameters.
     * @param query - TypedQuery fom setting parameters value
     */
    public <T> void setValuesToRegisteredParameters(TypedQueryImpl<T> query) {
        for(String name: parameters.keySet()){
            query.setParameter(name,parameters.get(name).getValue());
        }
    }

    private String generateParameterName(){
        return "generatedName"+ parameters.size();
    }
}
