/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.owl2query.model.GroundTerm;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.ResultBinding;
import cz.cvut.kbss.owl2query.model.Variable;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class QueryResultGenerator {

    private final OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    QueryResult<OWLObject> generate(List<String> variableNames, List<List<Object>> values) {
        final List<Variable<OWLObject>> vars = variableNames.stream().map(name -> {
            final Variable<OWLObject> var = mock(Variable.class);
            when(var.getName()).thenReturn(name);
            return var;
        }).collect(Collectors.toList());
        final QueryResult<OWLObject> result = new QueryResultImpl(vars);
        initData(result, values);
        return result;
    }

    private void initData(QueryResult<OWLObject> result, List<List<Object>> values) {
        if (values.isEmpty()) {
            return;
        }
        assert result.getResultVars().size() == values.get(0).size();
        for (List<Object> row : values) {
            final ResultBinding<OWLObject> binding = new ResultBindingImpl();
            for (int i = 0; i < row.size(); i++) {
                final Object value = row.get(i);
                if (value == null) {
                    binding.put(result.getResultVars().get(i), null);
                    continue;
                }
                final GroundTerm<OWLObject> gt = mock(GroundTerm.class);
                final OWLObject owlValue;
                if (value instanceof URI) {
                    owlValue = dataFactory.getOWLNamedIndividual(IRI.create(value.toString()));
                } else {
                    owlValue = OwlapiUtils.createOWLLiteralFromValue(value, "en");
                }
                when(gt.getWrappedObject()).thenReturn(owlValue);
                binding.put(result.getResultVars().get(i), gt);
            }
            result.add(binding);
        }
    }

    private static class QueryResultImpl implements QueryResult<OWLObject> {

        private final List<ResultBinding<OWLObject>> bindings = new ArrayList<>();
        private final List<Variable<OWLObject>> variables;

        private QueryResultImpl(List<Variable<OWLObject>> variables) {
            this.variables = variables;
        }

        @Override
        public boolean add(ResultBinding<OWLObject> resultBinding) {
            return bindings.add(resultBinding);
        }

        @Override
        public List<Variable<OWLObject>> getResultVars() {
            return Collections.unmodifiableList(variables);
        }

        @Override
        public boolean isDistinct() {
            return false;
        }

        @Override
        public boolean isEmpty() {
            return bindings.isEmpty();
        }

        @Override
        public int size() {
            return bindings.size();
        }

        @Override
        public Iterator<ResultBinding<OWLObject>> iterator() {
            return bindings.iterator();
        }
    }

    private static class ResultBindingImpl implements ResultBinding<OWLObject> {

        private final Map<Variable<OWLObject>, GroundTerm<OWLObject>> map = new HashMap<>();

        @Override
        public ResultBinding<OWLObject> clone() {
            return null;
        }

        @Override
        public int size() {
            return map.size();
        }

        @Override
        public boolean isEmpty() {
            return map.isEmpty();
        }

        @Override
        public boolean containsKey(Object key) {
            return map.containsKey(key);
        }

        @Override
        public boolean containsValue(Object value) {
            return map.containsValue(value);
        }

        @Override
        public GroundTerm<OWLObject> get(Object key) {
            return map.get(key);
        }

        @Override
        public GroundTerm<OWLObject> put(Variable<OWLObject> key, GroundTerm<OWLObject> value) {
            return map.put(key, value);
        }

        @Override
        public GroundTerm<OWLObject> remove(Object key) {
            return map.remove(key);
        }

        @Override
        public void putAll(Map<? extends Variable<OWLObject>, ? extends GroundTerm<OWLObject>> m) {
            map.putAll(m);
        }

        @Override
        public void clear() {
            map.clear();
        }

        @Override
        public Set<Variable<OWLObject>> keySet() {
            return map.keySet();
        }

        @Override
        public Collection<GroundTerm<OWLObject>> values() {
            return map.values();
        }

        @Override
        public Set<Entry<Variable<OWLObject>, GroundTerm<OWLObject>>> entrySet() {
            return map.entrySet();
        }
    }
}
