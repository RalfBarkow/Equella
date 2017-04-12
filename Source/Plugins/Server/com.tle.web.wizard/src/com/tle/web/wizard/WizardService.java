package com.tle.web.wizard;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.dytech.devlib.PropBagEx;
import com.dytech.edge.common.FileNode;
import com.dytech.edge.common.ScriptContext;
import com.tle.annotation.NonNullByDefault;
import com.tle.beans.item.ItemId;
import com.tle.beans.item.ItemKey;
import com.tle.beans.item.ItemPack;
import com.tle.beans.workflow.WorkflowStatus;
import com.tle.common.scripting.ScriptEvaluator;
import com.tle.core.filesystem.StagingFile;
import com.tle.core.wizard.controls.HTMLControl;
import com.tle.core.wizard.controls.WizardPage;
import com.tle.core.workflow.operations.WorkflowOperation;
import com.tle.web.sections.SectionInfo;
import com.tle.web.viewable.ViewableItem;

@NonNullByDefault
public interface WizardService extends ScriptEvaluator
{
	void doSave(WizardState state, boolean unlock, WorkflowOperation... operations);

	void doOperations(WizardState state, WorkflowOperation... operations);

	/**
	 * Merges the values from each target/value pair in to the document bag.
	 * This method has not been completely tested! The new format for the fixed
	 * meta data element should look something like the following: <wizard> ...
	 * <fixedmetadata> <data target="/item/somewhere" value="some value"> <data
	 * target="/workflow/registered" value="true"> ... </fixedmetadata>
	 * </wizard>
	 * 
	 * @dytech.jira see Jira Issue TLE-689 :
	 *              http://apps.dytech.com.au/jira/browse/TLE-689
	 */
	void transferMetaData(WizardState state, PropBagEx dest);

	void reload(WizardState state, boolean bEdit);

	void reloadSaveAndContinue(WizardState state);

	void newVersion(WizardState state);

	/*
	 * (non-Javadoc)
	 * @see com.dytech.edge.wizard.LERepository#getUserUUID()
	 */
	String getUserUUID();

	void previewHtml(String html);

	void selectTopLevelFilesAsAttachments(WizardState state);

	List<FileNode> linearizeFileTree(List<FileNode> vec, String prefix);

	String getOriginalUrl();

	FileNode getFileTree(WizardState state, String path);

	int checkPages(WizardState state);

	boolean checkDuplicateXpathValue(WizardState state, String xpath, Collection<String> values, boolean canAccept);

	void checkDuplicateUrls(WizardState state, String[] urls);

	ScriptContext createScriptContext(WizardState state, WizardPage page, HTMLControl control,
		Map<String, Object> attributes);

	/**
	 * For stateless wizards like PowerSearch
	 * 
	 * @param pack
	 * @param page
	 * @param control
	 * @param attributes
	 * @return
	 */
	ScriptContext createScriptContext(ItemPack pack, WizardPage page, HTMLControl control,
		Map<String, Object> attributes);

	WizardState loadItem(ItemKey key, boolean edit, boolean redraft);

	WizardState newItem(String itemdefUuid);

	/**
	 * @param collectionUuid The UUID of the collection to create a new item in
	 * @param initialXml The initial XML of the wizard
	 * @param staging Not required. If you wish to use a pre-existing staging
	 *            area then by all means supply one.
	 * @return A wizard state to be stored in the session (use
	 *         addToSession(WizardStateInterface) ) @
	 */
	WizardState newItem(String collectionUuid, PropBagEx initialXml, StagingFile staging);

	/**
	 * @param state The old wizard state, a fresh load of the old item.
	 * @param newItemDefUuid The new item definition to move the item to.
	 *            (doesn't have to be different, or leave it null to remain
	 *            unchanged)
	 * @param transform Optional. This is the name of the schema import
	 *            transform on the newItemDef's schema.
	 * @param copyAttachments Bring the attachments across as well as the
	 *            metadata.
	 * @return A new wizard state initialised with a clone of the item with the
	 *         new itemDef. The unified XML will have been transformed if a
	 *         transform was supplied.
	 */
	WizardState cloneItem(WizardState state, String newItemdefUuid, String transform, boolean copyAttachments);

	/**
	 * Not much more than an Edit really, but the transform can still apply.
	 * 
	 * @param itemkey The key for the item that's being moved
	 * @param newItemDefUuid The new item definition to move the item to.
	 * @param transform Optional. This is the name of the schema import
	 *            transform on the newItemDef's schema.
	 * @return A new wizard state initialised with the item with the new
	 *         itemDef. The unified XML will have been transformed if a
	 *         transform was supplied. @
	 */
	WizardState moveItem(ItemId itemkey, String newItemDefUuid, String transform);

	List<WizardInfo> listWizardsInSession();

	void cancelEdit(WizardState state);

	void unlock(WizardState state);

	List<WebWizardPage> getWizardPages(WizardState state);

	WorkflowStatus getWorkflowStatus(WizardState state);

	ViewableItem createViewableItem(WizardStateInterface state);

	void addToSession(SectionInfo info, WizardStateInterface state, boolean resumable);

	<T extends WizardStateInterface> T getFromSession(SectionInfo info, String id);

	void removeFromSession(SectionInfo info, String id, boolean cancelEdit);

	void updateSession(SectionInfo info, WizardStateInterface state);

	Object getThreadLock();
}