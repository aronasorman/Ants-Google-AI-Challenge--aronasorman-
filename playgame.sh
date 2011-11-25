TOOLS_DIR=../ant_tools
python $TOOLS_DIR/playgame.py "./MyBot" "python $TOOLS_DIR/sample_bots/python/HunterBot.py" --map_file $TOOLS_DIR/maps/example/tutorial1.map --log_dir game_logs --turns 90 --player_seed 7 --scenario --verbose -e --food none
