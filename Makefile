PORT=10080
VERSION := $(shell cat VERSION)

BASE=src/React.java                                           \
     src/ReactComponent.java                                  \
     src/ReactWidget.java                                     \
     src/ReactComponentFactory.java                           \
     src/CodeChangeListener.java                              \
     src/StyleSheetListener.java                              \
     src/StyleSheet.java                                      \
     src/ServerConnection.java                                \
     src/ReactLeafComponent.java                              \
     src/ReactComponentFactoryConfiguration.java

UI=  src/ui/Panel.java                                        \
     src/ui/Broken.java                                       \
     src/ui/Button.java                                       \
     src/ui/Field.java                                        \
     src/ui/PopupMenu.java                                    \
     src/ui/MenuItem.java                                     \
     src/ui/ComboBox.java                                     \
     src/ui/ComboItem.java                                    \
     src/ui/InputWidget.java                                  \
     src/ui/InputWidgetListener.java                          \
     src/ui/InputWidgetVerifier.java                          \
     src/ui/RadioButton.java                                  \
     src/ui/TextField.java                                    \
     src/ui/CheckBox.java                                     \
     src/ui/PasswordField.java                                \
     src/ui/Label.java                                        \
     src/ui/Table.java                                        \
     src/ui/TableHeader.java                                  \
     src/ui/Row.java                                          \
     src/ui/RowChangeListener.java                            \
     src/ui/TabbedPane.java                                   \
     src/ui/Tab.java                                          \
     src/ui/List.java                                         \
     src/ui/ListItem.java                                     \
     src/ui/TextArea.java                                     \
     src/ui/Tree.java                                         \
     src/ui/EditorPane.java                                   \
     src/ui/Frame.java                                        \
     src/ui/ReactApp.java                                     \
     src/ui/ReactPanel.java                                   \
     src/ui/DefaultReactComponentFactoryConfiguration.java

ENGINE= src/prolog/Predicate_remove_child.java                \
	src/prolog/Predicate_append_child.java                \
        src/prolog/Predicate_insert_before.java               \
        src/prolog/Predicate_replace_child.java               \
        src/prolog/Predicate_child_nodes.java                 \
        src/prolog/Predicate_create_element.java              \
        src/prolog/Predicate_create_text_node.java            \
        src/prolog/Predicate_parent_node.java                 \
        src/prolog/Predicate_node_type.java                   \
	src/prolog/Predicate_set_vdom_properties.java         \
        src/prolog/Predicate_replace_node_data.java           \
        src/prolog/Predicate_destroy_widget.java              \
        src/prolog/Predicate_update_widget.java               \
	src/prolog/Predicate_init_widget.java                 \
	src/prolog/Predicate_get_this.java                    \
	src/prolog/Predicate_get_store_state.java             \
	src/prolog/Predicate_get_state.java                   \
	src/prolog/Predicate_state_to_term.java               \
	src/prolog/Predicate_bubble_event.java                \
	src/prolog/Engine.java                                \
        src/prolog/PrologState.java                           \
        src/prolog/PrologObject.java                          \
        src/prolog/ReactEnvironment.java                      \
	src/prolog/FluxDispatcher.java                        \
	src/prolog/FluxStore.java                             \
	src/prolog/Predicate_java_println.java                \
	src/prolog/Predicate_format.java                      \
	src/prolog/Predicate_findall.java                     \
	src/prolog/Predicate_upcase_atom.java                 \
        src/prolog/Predicate_on_server.java                   \
	src/prolog/Predicate_raise_event.java                 \
	src/prolog/Predicate_widget_id.java                   \
	src/prolog/Predicate_wait_for.java

BOILERPLATE = src/boilerplate.pl                              \
	      src/vdiff.pl

JAVA_SRC=$(BASE) $(UI) $(ENGINE)




all:	client

.src:	$(JAVA_SRC) Makefile
	echo $(JAVA_SRC) > $@

build:
	mkdir build

.PHONY: client package

client: dist/proactive.jar

ifeq ($(OS), Windows_NT)
CLASSPATH_SEPARATOR=\;
else
CLASSPATH_SEPARATOR=:
endif

dist/proactive.jar:	.src build $(BOILERPLATE)
	javac -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar -Xlint:unchecked @.src -d build
	jar cvf dist/proactive.jar -C build/ . -C src boilerplate.pl -C src vdiff.pl

run-client:	client
	java -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar${CLASSPATH_SEPARATOR}dist/proactive.jar org.proactive.React "http://localhost:${PORT}/react" "App"

run-server:
	swipl -f src/server.pl -g "start_react_server(${PORT}), ['demo/App']"

package: dist/proactive.jar src/jsx.pl src/vdiff.pl src/react.pl src/dom.pl
	mkdir -p package/lib package/src
	cp dist/proactive.jar package/lib/proactive-${VERSION}.jar
	cp src/jsx.pl src/vdiff.pl src/react.pl src/dom.pl package/src/
	cd package && zip -r ../proactive-${VERSION}.zip lib src
	rm -rf package

clean:
	rm -rf build
	rm -f dist/proactive.jar
	rm -f .src
