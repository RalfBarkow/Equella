package com.tle.admin.gui.item;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import net.miginfocom.swing.MigLayout;

import com.tle.beans.item.VersionSelection;
import com.tle.common.applet.gui.AppletGuiUtils.BetterGroup;
import com.tle.common.i18n.CurrentLocale;

@SuppressWarnings("nls")
public class VersionSelectionConfig extends JPanel
{
	private final BetterGroup<JRadioButton, VersionSelection> choices = new BetterGroup<JRadioButton, VersionSelection>(
		true);

	public VersionSelectionConfig(boolean showInstitutionDefaultChoice)
	{
		setLayout(new MigLayout("wrap 1"));

		if( showInstitutionDefaultChoice )
		{
			add(newChoice("institutiondefault", VersionSelection.INSTITUTION_DEFAULT));
		}
		add(newChoice("forcecurrent", VersionSelection.FORCE_CURRENT));
		add(newChoice("forcelatest", VersionSelection.FORCE_LATEST));
		add(new JLabel(s("allowuserchoice")));
		add(newChoice("defaultcurrent", VersionSelection.DEFAULT_TO_CURRENT), "gapleft 30");
		add(newChoice("defaultlatest", VersionSelection.DEFAULT_TO_LATEST), "gapleft 30");

		choices.selectButtonByValue(showInstitutionDefaultChoice ? VersionSelection.INSTITUTION_DEFAULT
			: VersionSelection.FORCE_CURRENT);
	}

	public void load(VersionSelection vs)
	{
		choices.selectButtonByValue(vs);
	}

	public VersionSelection save()
	{
		return choices.getSelectedValue();
	}

	private JRadioButton newChoice(String keyPart, VersionSelection value)
	{
		final JRadioButton rb = new JRadioButton(s(keyPart));
		choices.addButton(rb, value);
		return rb;
	}

	private String s(String keyPart)
	{
		return CurrentLocale.get("com.tle.admin.gui.item.versionselectionconfig." + keyPart);
	}
}