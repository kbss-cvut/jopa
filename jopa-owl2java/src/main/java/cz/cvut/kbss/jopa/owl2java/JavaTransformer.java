/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.*;
import cz.cvut.kbss.jopa.ic.api.AtomicSubClassConstraint;
import cz.cvut.kbss.jopa.ic.api.DataParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.ObjectParticipationConstraint;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.owl2java.cli.Option;
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import cz.cvut.kbss.jopa.owl2java.exception.OWL2JavaException;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.text.Normalizer;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static cz.cvut.kbss.jopa.owl2java.Constants.*;

public class JavaTransformer {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2JavaTransformer.class);

    private static final String[] KEYWORDS = {"abstract",
            "assert",
            "boolean",
            "break",
            "byte",
            "case",
            "catch",
            "char",
            "class",
            "const",
            "continue",
            "default",
            "do",
            "double",
            "else",
            "enum",
            "extends",
            "final",
            "finally",
            "float",
            "for",
            "goto",
            "if",
            "implements",
            "import",
            "instanceof",
            "int",
            "interface",
            "long",
            "native",
            "new",
            "package",
            "private",
            "protected",
            "public",
            "return",
            "short",
            "static",
            "strictfp",
            "super",
            "switch",
            "synchronized",
            "this",
            "throw",
            "throws",
            "transient",
            "try",
            "void",
            "volatile",
            "while"};

    private static final String PREFIX_STRING = "s_";
    private static final String PREFIX_CLASS = "c_";
    private static final String PREFIX_PROPERTY = "p_";
    private static final String PREFIX_INDIVIDUAL = "i_";
    private static final String PREFIX_DATATYPE = "d_";

    private JDefinedClass voc;
    private final Map<OWLEntity, JFieldRef> entities = new HashMap<>();

    private final Map<OWLClass, JDefinedClass> classes = new HashMap<>();

    private final TransformationConfiguration configuration;

    JavaTransformer(TransformationConfiguration configuration) {
        this.configuration = configuration;
    }

    private static String validJavaIDForIRI(final IRI iri) {
        if (iri.getFragment() != null) {
            return validJavaID(iri.getFragment());
        } else {
            int x = iri.toString().lastIndexOf("/");
            return validJavaID(iri.toString().substring(x + 1));
        }
    }

    private static String validJavaID(final String s) {
        String res = s.trim().replace("-", "_").replace("'", "_quote_").replace(".", "_dot_").replace(',', '_');
        // Replace non-ASCII characters with ASCII ones
        res = Normalizer.normalize(res, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
        if (Arrays.binarySearch(KEYWORDS, res) >= 0) {
            res = "_" + res;
        }
        return res;
    }

    private static JFieldVar addField(final String name, final JDefinedClass cls,
                                      final JType fieldType) {
        String newName = name;

        int i = 0;
        while (cls.fields().containsKey(newName)) {
            newName = name + "" + (++i);
        }

        final JFieldVar fvId = cls.field(JMod.PROTECTED, fieldType, newName);
        final String fieldName = fvId.name().substring(0, 1).toUpperCase() + fvId.name().substring(1);
        final JMethod mSetId = cls.method(JMod.PUBLIC, void.class, "set" + fieldName);
        final JVar v = mSetId.param(fieldType, fvId.name());
        mSetId.body().assign(JExpr._this().ref(fvId), v);
        final JMethod mGetId = cls.method(JMod.PUBLIC, fieldType, "get" + fieldName);
        mGetId.body()._return(fvId);
        return fvId;
    }

    /**
     * Generates an object model consisting of JOPA entity classes and a vocabulary file from the specified ontology and
     * context definition.
     *
     * @param ontology Ontology from which the model is generated
     * @param context  Context information
     * @return The generated object model
     */
    public ObjectModel generateModel(final OWLOntology ontology, final ContextDefinition context) {
        try {
            final JCodeModel cm = new JCodeModel();
            voc = createVocabularyClass(cm);
            generateVocabulary(ontology, cm, context);
            _generateModel(ontology, cm, context, modelPackageName());
            return new ObjectModel(cm);
        } catch (JClassAlreadyExistsException e) {
            throw new OWL2JavaException("Transformation FAILED.", e);
        }
    }

    private String modelPackageName() {
        final String packageConfig = configuration.getPackageName();
        final StringBuilder sb = new StringBuilder(packageConfig);
        if (!packageConfig.isEmpty()) {
            sb.append(PACKAGE_SEPARATOR);
        }
        sb.append(MODEL_PACKAGE);
        return sb.toString();
    }

    private JDefinedClass createVocabularyClass(JCodeModel codeModel)
            throws JClassAlreadyExistsException {
        final String packageName = configuration.getPackageName();
        final String className =
                packageName.isEmpty() ? VOCABULARY_CLASS : packageName + PACKAGE_SEPARATOR + VOCABULARY_CLASS;
        final JDefinedClass cls = codeModel._class(className);
        generateAuthorshipDoc(cls);
        return cls;
    }

    private static void generateAuthorshipDoc(JDocCommentable javaElem) {
        javaElem.javadoc().add("This class was generated by OWL2Java " + VERSION);
    }

    /**
     * Generates only vocabulary of the loaded ontology.
     *
     * @param ontology Ontology from which the vocabulary should be generated
     * @param context  Integrity constraints context, if null is supplied, the whole ontology is interpreted as
     *                 integrity constraints.
     * @return The generated object model
     */
    public ObjectModel generateVocabulary(final OWLOntology ontology, ContextDefinition context) {
        try {
            final JCodeModel cm = new JCodeModel();
            this.voc = createVocabularyClass(cm);
            generateVocabulary(ontology, cm, context);
            return new ObjectModel(cm);
        } catch (JClassAlreadyExistsException e) {
            throw new OWL2JavaException("Vocabulary generation FAILED, because the Vocabulary class already exists.",
                    e);
        }
    }

    private void generateObjectProperty(final OWLOntology ontology,
                                        final JCodeModel cm,
                                        final ContextDefinition context,
                                        final String pkg,
                                        final OWLClass clazz,
                                        final JDefinedClass subj,
                                        final org.semanticweb.owlapi.model.OWLObjectProperty prop) {
        final ClassObjectPropertyComputer comp = new ClassObjectPropertyComputer(
                clazz,
                prop,
                context.set,
                ontology
        );

        if (Card.NO != comp.getCard()) {
            JClass filler = ensureCreated(pkg, cm, comp.getFiller(), ontology);
            final String fieldName = validJavaIDForIRI(prop.getIRI());

            switch (comp.getCard()) {
                case MULTIPLE:
                    filler = cm.ref(java.util.Set.class).narrow(filler);
                    break;
                case SIMPLELIST:
                case LIST:
                    filler = cm.ref(java.util.List.class).narrow(filler);
                    break;
                default:
                    break;
            }

            final JFieldVar fv = addField(fieldName, subj, filler);
            generateJavadoc(ontology, prop, fv);

            if (comp.getCard().equals(Card.SIMPLELIST)) {
                fv.annotate(Sequence.class).param("type", SequenceType.simple);
            }


            fv.annotate(OWLObjectProperty.class).param("iri", entities.get(prop));

            JAnnotationArrayMember use = null;
            for (ObjectParticipationConstraint ic : comp.getParticipationConstraints()) {
                if (use == null) {
                    use = fv.annotate(ParticipationConstraints.class).paramArray("value");
                }
                JAnnotationUse u = use.annotate(ParticipationConstraint.class)
                        .param("owlObjectIRI", entities.get(ic.getObject()));
                setParticipationConstraintCardinality(u, ic);
            }
        }
    }

    private static void setParticipationConstraintCardinality(JAnnotationUse u,
                                                              cz.cvut.kbss.jopa.ic.api.ParticipationConstraint<?, ?> ic) {
        if (ic.getMin() != 0) {
            u.param("min", ic.getMin());
        }
        if (ic.getMin() != -1) {
            u.param("max", ic.getMax());
        }
    }

    private void generateDataProperty(final OWLOntology ontology,
                                      final JCodeModel cm,
                                      final ContextDefinition context,
                                      final OWLClass clazz,
                                      final JDefinedClass subj,
                                      final org.semanticweb.owlapi.model.OWLDataProperty prop) {
        final ClassDataPropertyComputer comp = new ClassDataPropertyComputer(
                clazz,
                prop,
                context.set,
                ontology
        );

        if (Card.NO != comp.getCard()) {

            final JType obj = cm._ref(resolveFieldType(comp.getFiller()));

            final String fieldName = validJavaIDForIRI(prop.getIRI());

            JFieldVar fv;

            switch (comp.getCard()) {
                case MULTIPLE:
                    fv = addField(fieldName, subj, cm.ref(java.util.Set.class).narrow(obj));
                    break;
                case ONE:
                    fv = addField(fieldName, subj, obj);
                    break;
                default:
                    throw new OWL2JavaException("Unsupported data property cardinality type " + comp.getCard());
            }
            generateJavadoc(ontology, prop, fv);

            fv.annotate(OWLDataProperty.class).param("iri", entities.get(prop));

            JAnnotationArrayMember use = null;
            for (DataParticipationConstraint ic : comp.getParticipationConstraints()) {
                if (use == null) {
                    use = fv.annotate(ParticipationConstraints.class).paramArray("value");
                }

                JAnnotationUse u = use.annotate(ParticipationConstraint.class)
                        .param("owlObjectIRI", comp.getFiller().getIRI().toString());

                setParticipationConstraintCardinality(u, ic);
            }
        }
    }

    private Class<?> resolveFieldType(OWLDatatype datatype) {
        final Class<?> cls = DatatypeTransformer.transformOWLType(datatype);
        if (MultilingualString.class.equals(cls) && !configuration.shouldPreferMultilingualStrings()) {
            return String.class;
        }
        return cls;
    }

    private void _generateModel(final OWLOntology ontology, final JCodeModel cm,
                                final ContextDefinition context, final String pkg) {
        LOG.info("Generating model ...");
        final PropertiesType propertiesType = configuration.getPropertiesType();

        context.classes.add(ontology.getOWLOntologyManager().getOWLDataFactory().getOWLThing());

        for (final OWLClass clazz : context.classes) {
            LOG.info("  Generating class '{}'.", clazz);
            final JDefinedClass subj = ensureCreated(pkg, cm, clazz, ontology);

            final AtomicBoolean extendClass = new AtomicBoolean(false);
            context.set.getClassIntegrityConstraints(clazz).stream()
                    .filter(ic -> ic instanceof AtomicSubClassConstraint).forEach(ic -> {
                        final AtomicSubClassConstraint icc = (AtomicSubClassConstraint) ic;
                        subj._extends(ensureCreated(pkg, cm, icc.getSupClass(), ontology
                        ));
                        extendClass.set(true);
                    });

            if (!extendClass.get())
                addCommonClassFields(cm, subj, propertiesType);
            for (final org.semanticweb.owlapi.model.OWLObjectProperty prop : context.objectProperties) {
                generateObjectProperty(ontology, cm, context, pkg, clazz, subj, prop);
            }

            for (org.semanticweb.owlapi.model.OWLDataProperty prop : context.dataProperties) {
                generateDataProperty(ontology, cm, context, clazz, subj, prop);
            }
        }
    }

    private void generateVocabulary(final OWLOntology o, final JCodeModel cm, ContextDefinition context) {
        final Collection<OWLEntity> col = new LinkedHashSet<>();
        col.add(o.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
        col.addAll(context.classes);
        col.addAll(context.objectProperties);
        col.addAll(context.dataProperties);
        col.addAll(context.annotationProperties);
        col.addAll(context.individuals);

        generateOntologyIrisConstants(o.getOWLOntologyManager());

        final Set<IRI> visitedProperties = new HashSet<>(col.size());

        for (final OWLEntity c : col) {
            final Optional<String> prefix = resolveFieldPrefix(c, visitedProperties);
            if (!prefix.isPresent()) {
                continue;
            }
            final String sFieldName = ensureUniqueIdentifier(
                    PREFIX_STRING + prefix.get() + validJavaIDForIRI(c.getIRI()));

            final JFieldVar fv1 = voc.field(JMod.PUBLIC | JMod.STATIC
                    | JMod.FINAL, String.class, sFieldName, JExpr.lit(c.getIRI().toString()));
            if (configuration.shouldGenerateOwlapiIris()) {
                voc.field(JMod.PUBLIC | JMod.STATIC | JMod.FINAL, IRI.class,
                        sFieldName.substring(PREFIX_STRING.length()),
                        cm.ref(IRI.class).staticInvoke("create").arg(fv1));
            }
            generateJavadoc(o, c, fv1);
            entities.put(c, voc.staticRef(fv1));
        }
    }

    private void generateOntologyIrisConstants(OWLOntologyManager ontologyManager) {
        // Get only unique ontology IRIs sorted
        final List<IRI> ontologyIris = ontologyManager.ontologies().map(o -> o.getOntologyID().getOntologyIRI())
                .filter(Optional::isPresent).map(Optional::get).distinct()
                .sorted(Comparator.comparing(IRI::getIRIString))
                .collect(Collectors.toList());
        ontologyIris.forEach(iri -> {
            final String fieldName = ensureUniqueIdentifier("ONTOLOGY_IRI_" + validJavaIDForIRI(iri));
            voc.field(JMod.PUBLIC | JMod.STATIC | JMod.FINAL, String.class, fieldName, JExpr.lit(iri.toString()));
        });
    }

    private static Optional<String> resolveFieldPrefix(OWLEntity c, Set<IRI> visitedProperties) {
        if (c.isOWLClass()) {
            return Optional.of(PREFIX_CLASS);
        } else if (c.isOWLDatatype()) {
            return Optional.of(PREFIX_DATATYPE);
        } else if (c.isOWLDataProperty() || c.isOWLObjectProperty() || c.isOWLAnnotationProperty()) {
            if (visitedProperties.contains(c.getIRI())) {
                LOG.debug("Property with IRI {} already processed. Skipping.", c.getIRI());
                return Optional.empty();
            }
            visitedProperties.add(c.getIRI());
            return Optional.of(PREFIX_PROPERTY);
        } else if (c.isOWLNamedIndividual()) {
            return Optional.of(PREFIX_INDIVIDUAL);
        }
        return Optional.of("");
    }

    private String ensureUniqueIdentifier(String id) {
        final StringBuilder sb = new StringBuilder(id);
        while (voc.fields().containsKey(sb.toString())) {
            sb.append("_A");
        }
        return sb.toString();
    }

    /**
     * Generates Javadoc from rdfs:comment annotation (if present).
     *
     * @param ontology  Ontology from which the model/vocabulary is being generated
     * @param owlEntity Annotated entity
     * @param javaElem  Element to document with Javadoc
     * @return Whether the javadoc comment has been generated
     */
    private boolean generateJavadoc(OWLOntology ontology, OWLEntity owlEntity, JDocCommentable javaElem) {
        if (!configuration.shouldGenerateJavadoc()) {
            return false;
        }
        final List<OWLAnnotation> comments = EntitySearcher.getAnnotations(owlEntity, ontology)
                .filter(a -> a.getProperty().isComment() && a.getValue().isLiteral()).collect(Collectors.toList());
        final Optional<OWLAnnotation> langComment = comments.stream().filter(a -> a.getValue().asLiteral()
                .map(l -> l.hasLang(LANGUAGE)).orElse(false)).findFirst();
        // First try finding a comment with a matching language tag
        if (langComment.isPresent()) {
            langComment.flatMap(a -> a.getValue().asLiteral())
                    .ifPresent(lit -> javaElem.javadoc().add(lit.getLiteral()));
            return true;
        }
        // If there is none such, just use the first available one
        if (!comments.isEmpty()) {
            OWLAnnotation anyComment = comments.get(0);
            anyComment.getValue().asLiteral().ifPresent(lit -> javaElem.javadoc().add(lit.getLiteral()));
            return true;
        }
        return false;
    }

    private JDefinedClass create(final String pkg, final JCodeModel cm, final OWLClass clazz,
                                 final OWLOntology ontology) {
        JDefinedClass cls;

        String name = pkg + PACKAGE_SEPARATOR + javaClassId(ontology, clazz);

        try {
            cls = cm._class(name);

            cls.annotate(cz.cvut.kbss.jopa.model.annotations.OWLClass.class).param("iri", entities.get(clazz));
            cls._implements(Serializable.class);

            generateClassJavadoc(ontology, clazz, cls);
        } catch (JClassAlreadyExistsException e) {
            LOG.trace("Class already exists. Using the existing version. {}", e.getMessage());
            cls = cm._getClass(name);
        }
        return cls;
    }

    /**
     * Add common properties such as id and type
     */
    private void addCommonClassFields(final JCodeModel cm, final JDefinedClass cls,
                                      final PropertiesType propertiesType) {
        // @Id(generated = true) protected String id;
        final JClass ftId = cm.ref(String.class);
        final JFieldVar fvId = addField("id", cls, ftId);
        JAnnotationUse a = fvId.annotate(Id.class);
        a.param("generated", true);

        // RDFS label
        final JClass ftLabel = cm.ref(String.class);
        final JFieldVar fvLabel = addField("name", cls, ftLabel);
        fvLabel.annotate(OWLAnnotationProperty.class).param("iri", cm.ref(RDFS.class).staticRef("LABEL"));

        // DC description
        final JClass ftDescription = cm.ref(String.class);
        final JFieldVar fvDescription = addField("description", cls, ftDescription);
        fvDescription.annotate(OWLAnnotationProperty.class)
                .param("iri", cm.ref(DC.Elements.class).staticRef("DESCRIPTION"));

        // @Types Set<String> types;
        final JClass ftTypes = cm.ref(Set.class).narrow(String.class);
        final JFieldVar fvTypes = addField("types", cls, ftTypes);
        fvTypes.annotate(Types.class);

        // @Properties public final Map<String,Set<String>> properties;
        final Class propertiesTypeC = (propertiesType == PropertiesType.object ? Object.class : String.class);
        final JClass ftProperties = cm.ref(Map.class)
                .narrow(cm.ref(String.class), cm.ref(Set.class).narrow(propertiesTypeC));
        final JFieldVar fvProperties = addField("properties", cls, ftProperties);
        fvProperties.annotate(Properties.class);

        generateToStringMethod(cls, fvId.name(), fvLabel.name());
    }

    private String javaClassId(OWLOntology ontology, OWLClass owlClass) {
        final Optional<OWLAnnotation> res = EntitySearcher.getAnnotations(owlClass, ontology)
                .filter(a -> isJavaClassNameAnnotation(a) &&
                        a.getValue().isLiteral()).findFirst();
        if (res.isPresent()) {
            return res.get().getValue().asLiteral().get().getLiteral();
        } else {
            return toJavaNotation(validJavaIDForIRI(owlClass.getIRI()));
        }
    }

    private void generateClassJavadoc(OWLOntology ontology, OWLEntity owlEntity, JDocCommentable javaElem) {
        final boolean generated = generateJavadoc(ontology, owlEntity, javaElem);
        if (generated) {
            javaElem.javadoc().add("\n\n");
        }
        generateAuthorshipDoc(javaElem);
    }

    private static void generateToStringMethod(JDefinedClass cls, String idFieldName, String labelFieldName) {
        final JMethod toString = cls.method(JMod.PUBLIC, String.class, "toString");
        toString.annotate(Override.class);
        final JBlock body = toString.body();
        JExpression expression = JExpr.lit(cls.name() + " {");
        expression = expression.plus(JExpr.ref(labelFieldName));
        expression = expression.plus(JExpr.lit("<")).plus(JExpr.ref(idFieldName)).plus(JExpr.lit(">"));
        expression = expression.plus(JExpr.lit("}"));

        body._return(expression);
    }

    private JDefinedClass ensureCreated(final String pkg, final JCodeModel cm, final OWLClass clazz,
                                        final OWLOntology ontology) {
        if (!classes.containsKey(clazz)) {
            classes.put(clazz, create(pkg, cm, clazz, ontology));
        }
        return classes.get(clazz);
    }

    private boolean isJavaClassNameAnnotation(OWLAnnotation a) {
        final String classNameProperty = (String) configuration.getCliParams()
                .valueOf(Option.JAVA_CLASSNAME_ANNOTATION.arg);
        return a.getProperty().getIRI()
                .equals(IRI.create(classNameProperty != null ? classNameProperty : Constants.P_CLASS_NAME));
    }

    /**
     * Converts a class name to the Java camel case notation
     *
     * @param className Generated class name
     * @return Converted class name
     */
    private static String toJavaNotation(String className) {
        StringBuilder result = new StringBuilder();
        for (String w : className.split("_")) {
            if (!w.isEmpty()) {
                result.append(w.substring(0, 1).toUpperCase()).append(w.substring(1));
            }
        }
        return result.toString();
    }
}
