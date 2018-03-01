/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.*;
import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.ic.api.AtomicSubClassConstraint;
import cz.cvut.kbss.jopa.ic.api.DataParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.ObjectParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import joptsimple.OptionSet;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

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
    private Map<OWLEntity, JFieldRef> entities = new HashMap<>();

    private Map<OWLClass, JDefinedClass> classes = new HashMap<>();

    private final OptionSet configuration;

    JavaTransformer(OptionSet configuration) {
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
        if (Arrays.binarySearch(KEYWORDS, res) >= 0) {
            res = "_" + res;
        }
        return res;
    }

    private JFieldVar addField(final String name, final JDefinedClass cls,
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

    public void generateModel(final OWLOntology ontology, final ContextDefinition context,
                              TransformationConfiguration transformConfig) {
        final String packageName = transformConfig.getPackageName();
        try {
            final JCodeModel cm = new JCodeModel();
            voc = cm._class(packageName + PACKAGE_SEPARATOR + VOCABULARY_CLASS);
            generateVocabulary(ontology, cm, context, transformConfig.shouldGenerateOwlapiIris());
            _generateModel(ontology, cm, context, packageName + PACKAGE_SEPARATOR + MODEL_PACKAGE + PACKAGE_SEPARATOR);
            writeOutModel(cm, transformConfig.getTargetDir());
        } catch (JClassAlreadyExistsException e1) {
            LOG.error("Transformation FAILED.", e1);
        } catch (IOException e) {
            LOG.error("File generation FAILED.", e);
        }
    }

    /**
     * Generates only vocabulary of the loaded ontology.
     *
     * @param ontology        Ontology from which the vocabulary should be generated
     * @param context         Integrity constraints context, if null is supplied, the whole ontology is interpreted as
     *                        integrity constraints.
     * @param transformConfig Configuration of the generation process
     */
    public void generateVocabulary(final OWLOntology ontology, ContextDefinition context,
                                   TransformationConfiguration transformConfig) {
        try {
            final JCodeModel cm = new JCodeModel();
            this.voc = cm._class(transformConfig.getPackageName() + PACKAGE_SEPARATOR + VOCABULARY_CLASS);
            generateVocabulary(ontology, cm, context, transformConfig.shouldGenerateOwlapiIris());
            writeOutModel(cm, transformConfig.getTargetDir());
        } catch (JClassAlreadyExistsException e) {
            LOG.error("Vocabulary generation FAILED, because the Vocabulary class already exists.", e);
        } catch (IOException e) {
            LOG.error("Vocabulary file generation FAILED.", e);
        }
    }

    private void writeOutModel(JCodeModel cm, String targetDir) throws IOException {
        final File file = new File(targetDir);
        file.mkdirs();
        cm.build(file);
    }

    private void _generateObjectProperty(final OWLOntology ontology,
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

        if (!Card.NO.equals(comp.getCard())) {
            JClass filler = ensureCreated(context, pkg, cm,
                    comp.getFiller(), ontology);
            final String fieldName = validJavaIDForIRI(prop.getIRI());

            switch (comp.getCard()) {
                case ONE:
                    break;
                case MULTIPLE:
                    filler = cm.ref(java.util.Set.class).narrow(filler);
                    break;
                case SIMPLELIST:
                case LIST:
                    filler = cm.ref(java.util.List.class).narrow(filler);
                    break;
            }

            final JFieldVar fv = addField(fieldName, subj, filler);

            if (comp.getCard().equals(Card.SIMPLELIST)) {
                fv.annotate(Sequence.class)
                  .param("type", SequenceType.simple);
            }


            fv.annotate(OWLObjectProperty.class).param("iri",
                    entities.get(prop));

            JAnnotationArrayMember use = null;
            for (ObjectParticipationConstraint ic : comp.getParticipationConstraints()) {
                if (use == null) {
                    use = fv.annotate(ParticipationConstraints.class).paramArray("value");
                }
                JAnnotationUse u = use.annotate(
                        ParticipationConstraint.class).param(
                        // "owlClassIRI",
                        // ic.getSubject().getIRI().toString()).param(
                        // "owlPropertyIRI",
                        // ic.getPredicate().getIRI().toString()).param(
                        "owlObjectIRI", entities.get(ic.getObject()));
                setParticipationConstraintCardinality(u, ic);
            }
        }
    }

    private void setParticipationConstraintCardinality(JAnnotationUse u,
                                                       cz.cvut.kbss.jopa.ic.api.ParticipationConstraint ic) {
        if (ic.getMin() != 0) {
            u.param("min", ic.getMin());
        }
        if (ic.getMin() != -1) {
            u.param("max", ic.getMax());
        }
    }

    private void _generateDataProperty(final OWLOntology ontology,
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

        if (!Card.NO.equals(comp.getCard())) {

            final JType obj = cm._ref(DatatypeTransformer.transformOWLType(comp.getFiller()));

            final String fieldName = validJavaIDForIRI(prop.getIRI());

            JFieldVar fv;

            if (Card.MULTIPLE.equals(comp.getCard())) {
                fv = addField(fieldName, subj, cm.ref(java.util.Set.class)
                                                 .narrow(obj));
            } else if (Card.ONE.equals(comp.getCard())) {
                fv = addField(fieldName, subj, obj);
            } else {
                assert false : "Unknown cardinality type";
                return;
            }

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

    private void _generateModel(final OWLOntology ontology, final JCodeModel cm,
                                final ContextDefinition context, final String pkg) {
        LOG.info("Generating model ...");

        context.classes.add(ontology.getOWLOntologyManager().getOWLDataFactory().getOWLThing());

        for (final OWLClass clazz : context.classes) {
            LOG.info("  Generating class '{}'.", clazz);
            final JDefinedClass subj = ensureCreated(context, pkg, cm, clazz, ontology);

            context.set.getClassIntegrityConstraints(clazz).forEach((ic) -> {
                if (ic instanceof AtomicSubClassConstraint) {
                    final AtomicSubClassConstraint icc = (AtomicSubClassConstraint) ic;
                    subj._extends(ensureCreated(context, pkg, cm, icc.getSupClass(), ontology));
                }
            });

            for (final org.semanticweb.owlapi.model.OWLObjectProperty prop : context.objectProperties) {
                _generateObjectProperty(ontology, cm, context, pkg, clazz, subj, prop);
            }

            for (org.semanticweb.owlapi.model.OWLDataProperty prop : context.dataProperties) {
                _generateDataProperty(ontology, cm, context, clazz, subj, prop);
            }
        }
    }

    private void generateVocabulary(final OWLOntology o, final JCodeModel cm, ContextDefinition context,
                                    boolean withOWLAPI) {
        final Collection<OWLEntity> col = new LinkedHashSet<>();
        col.add(o.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
        col.addAll(context.classes);
        col.addAll(context.objectProperties);
        col.addAll(context.dataProperties);
        col.addAll(context.annotationProperties);
        col.addAll(context.individuals);

        o.getOWLOntologyManager().ontologies().forEach(onto -> onto.getOntologyID().getOntologyIRI().ifPresent(iri -> {
            final String fieldName = ensureUniqueIdentifier("ONTOLOGY_IRI_" + validJavaIDForIRI(iri));
            voc.field(JMod.PUBLIC | JMod.STATIC | JMod.FINAL, String.class, fieldName, JExpr.lit(iri.toString()));
        }));

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
            if (withOWLAPI) {
                voc.field(JMod.PUBLIC | JMod.STATIC | JMod.FINAL, IRI.class,
                        sFieldName.substring(PREFIX_STRING.length()),
                        cm.ref(IRI.class).staticInvoke("create").arg(fv1));
            }
            entities.put(c, voc.staticRef(fv1));
        }
    }

    private Optional<String> resolveFieldPrefix(OWLEntity c, Set<IRI> visitedProperties) {
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
        while (voc.fields().keySet().contains(sb.toString())) {
            sb.append("_A");
        }
        return sb.toString();
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

    private JDefinedClass ensureCreated(final ContextDefinition ctx,
                                        final String pkg, final JCodeModel cm, final OWLClass clazz,
                                        final OWLOntology ontology) {
        if (classes.containsKey(clazz)) {
            return classes.get(clazz);
        }

        JDefinedClass cls;

        String name = pkg + javaClassId(ontology, clazz);

        try {
            cls = cm._class(name);

            cls.annotate(
                    cz.cvut.kbss.jopa.model.annotations.OWLClass.class)
               .param("iri", entities.get(clazz));

            final JDocComment dc = cls.javadoc();
            dc.add("This class was generated by the OWL2Java tool version " + VERSION);

            // if (clazz.equals(f.getOWLThing())) {
            // RDFS label
            final JClass ftLabel = cm.ref(String.class);
            final JFieldVar fvLabel = addField("name", cls, ftLabel);
            fvLabel.annotate(OWLAnnotationProperty.class).param("iri",
                    cm.ref(CommonVocabulary.class).staticRef("RDFS_LABEL"));

            // DC description
            final JClass ftDescription = cm.ref(String.class);
            final JFieldVar fvDescription = addField("description", cls, ftDescription);
            fvDescription.annotate(OWLAnnotationProperty.class).param("iri",
                    cm.ref(CommonVocabulary.class).staticRef("DC_DESCRIPTION"));

            // @Types Set<String> types;
            final JClass ftTypes = cm.ref(Set.class).narrow(String.class);
            final JFieldVar fvTypes = addField("types", cls, ftTypes);
            fvTypes.annotate(Types.class);

            // @Id public final String id;
            final JClass ftId = cm.ref(String.class);
            final JFieldVar fvId = addField("id", cls, ftId);
            JAnnotationUse a = fvId.annotate(Id.class);

            a.param("generated", true);

            // @Properties public final Map<String,Set<String>> properties;
            final JClass ftProperties = cm.ref(Map.class).narrow(
                    cm.ref(String.class),
                    cm.ref(Set.class).narrow(String.class));
            final JFieldVar fvProperties = addField("properties", cls,
                    ftProperties);
            fvProperties.annotate(Properties.class);
            // }

        } catch (JClassAlreadyExistsException e) {
            LOG.trace("Class already exists. Using the existing version. {}", e.getMessage());
            cls = cm._getClass(name);
        }
        classes.put(clazz, cls);

        return cls;
    }

    private boolean isJavaClassNameAnnotation(OWLAnnotation a) {
        final String classNameProperty = (String) configuration.valueOf(Param.JAVA_CLASSNAME_ANNOTATION.arg);
        return a.getProperty().getIRI()
                .equals(IRI.create(classNameProperty != null ? classNameProperty : Constants.P_CLASS_NAME));
    }

    /**
     * Converts a class name to the Java camel case notation
     *
     * @param className Generated class name
     * @return Converted class name
     */
    private String toJavaNotation(String className) {
        StringBuilder result = new StringBuilder();
        for (String w : className.split("_"))
            if (!w.isEmpty())
                result.append(w.substring(0, 1).toUpperCase()).append(w.substring(1));
        return result.toString();
    }
}