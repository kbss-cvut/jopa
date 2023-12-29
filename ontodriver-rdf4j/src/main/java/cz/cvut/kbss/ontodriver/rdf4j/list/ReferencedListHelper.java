package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.Value;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

class ReferencedListHelper {

    private final ValueConverter valueConverter;

    ReferencedListHelper(ValueConverter valueConverter) {this.valueConverter = valueConverter;}

    Collection<Value> toRdf4jValue(Assertion a, Object value) throws Rdf4jDriverException {
        if (value instanceof MultilingualString) {
            final MultilingualString mls = (MultilingualString) value;
            final List<Value> values = new ArrayList<>(mls.getValue().size());
            for (Map.Entry<String, String> e : mls.getValue().entrySet()) {
                values.add(valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(new LangString(e.getValue(), e.getKey()))));
            }
            return values;
        }
        return List.of(valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(value)));
    }
}
