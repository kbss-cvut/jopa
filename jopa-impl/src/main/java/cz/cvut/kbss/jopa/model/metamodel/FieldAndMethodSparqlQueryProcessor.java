package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.Sparql;
import cz.cvut.kbss.jopa.query.MemberQueryManager;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * WIP //TODO change to field
 */
public class FieldAndMethodSparqlQueryProcessor<X> {

    private static final Logger LOG = LoggerFactory.getLogger(FieldAndMethodSparqlQueryProcessor.class);

    private final FieldMappingValidator mappingValidator = new FieldMappingValidator();
    private final TypeBuilderContext<X> context;
    private final MetamodelBuilder metamodelBuilder;
    private final NamedQueryManager queryManager;

    private final MemberQueryManager memberQueryManager = new MemberQueryManager();

    public FieldAndMethodSparqlQueryProcessor(TypeBuilderContext<X> context, MetamodelBuilder metamodelBuilder) {
        this.context = context;
        this.metamodelBuilder = metamodelBuilder;
        this.queryManager = metamodelBuilder.getNamedQueryManager();
    }

    void processMethod(Method method) {
        LOG.trace("processing method: {}", method);

        Sparql sparqlAnnotation = method.getAnnotation(Sparql.class);

        if (sparqlAnnotation == null) {
            //not annotated
            return;
        }

        String nativeQuery = sparqlAnnotation.query();

        //TODO add query to metamodel/descriptor/query manager
        //TODO create and execute query between UnitOfWorkImpl-EntityConstructor
        //String queryName = property.getName();
        //queryManager.addNamedQuery(queryName, nativeQuery);

        memberQueryManager.addMethodQuery(method, nativeQuery);
    }

    void processField(Field field) {
        LOG.trace("processing field: {}", field);

        Sparql sparqlAnnotation = field.getAnnotation(Sparql.class);

        if (sparqlAnnotation == null) {
            //not annotated
            return;
        }

        String nativeQuery = sparqlAnnotation.query();

        memberQueryManager.addFieldQuery(field, nativeQuery);

        /*
        final Class<?> fieldValueCls = ClassFieldMetamodelProcessor.getFieldValueType(field);
        final PropertyAttributes propertyAtt = PropertyAttributes.create(field, mappingValidator, context);
        propertyAtt.resolve(field, metamodelBuilder, fieldValueCls);

        if (propertyAtt.isKnownOwlProperty()) {
            createAttribute(field, inference, propertyAtt);
        }
        */
    }

    public MemberQueryManager getMemberQueryManager() {
        return memberQueryManager;
    }
}
